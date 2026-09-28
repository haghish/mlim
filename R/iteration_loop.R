#' @title iteration_loop
#' @description runs imputation iteration loop to fully impute a dataframe
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom h2o h2o.init as.h2o h2o.predict h2o.ls
#'             h2o.removeAll h2o.rm h2o.shutdown h2o.get_automl h2o.getId
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit rnorm
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd


iteration_loop <- function(MI, dataNA, preimputed.data, data, bdata, boot, metrics, tolerance,
                           m, k, X, z, m.it,

                           # loop data
                           vars2impute,
                           allPredictors, preimpute, impute,
                           hierarchy = NULL,

                           # settings
                           error_metric, FAMILY, cv, tuning_time,
                           max_models,
                           autobalance,
                           seed, save, flush,
                           verbose, debug, report, sleep,

                           # saving settings
                           mem, orderedCols, ignore, maxiter,
                           matching, ignore.rank,
                           verbosity, error, cpu, max_ram, min_ram, shutdown, clean,
                           stochastic) {

  # ------------------------------------------------------------
  # Bootstrap
  #
  # Bootstrap from the original dataset, retain the original
  # missingness pattern, and preimpute the bootstrap dataset before
  # beginning the iterative procedure.
  # ============================================================
  bdataNA <- NULL

  if (boot) {
    rownames(data) <- seq_len(nrow(data))
    sampling_index <- sample(x = nrow(data), size = nrow(data), replace = TRUE)

    # Duplicated bootstrap observations are represented through weights.
    # This avoids identical duplicated rows being assigned independently
    # across cross-validation folds.
    dups <- bootstrapWeight(sampling_index)
    bdata <- data[seq_len(nrow(data)) %in% dups[, 1], , drop = FALSE]
    bdataNA <- is.na(bdata[, vars2impute, drop = FALSE])

    message("\n")
    bdata <- mlim.preimpute(data = bdata, preimpute = preimpute, seed = NULL)
    bdata[, "mlim_bootstrap_weights_column_"] <- dups[, 2]
  }

  # ------------------------------------------------------------
  # Initialize the iteration loop
  # ============================================================
  running <- TRUE

  if (debug) {
    md.log("data was sent to h2o cloud", date = debug, time = debug, trace = FALSE)
  }

  ITERATIONVARS <- vars2impute

  # Keep the original variable and predictor sets. Multilevel summary
  # variables are regenerated at the start of each global iteration.
  basePredictors <- allPredictors
  baseX <- X
  multilevel_variables <- character(0)

  if (!is.null(hierarchy)) {
    multilevel_source_variables <- setdiff(
      names(data),
      c(
        hierarchy,
        "mlim_bootstrap_weights_column_",
        "mlim_model_weights_column_"
      )
    )
  }
  else {
    multilevel_source_variables <- character(0)
  }

  # Update or add one column in an existing H2O working frame.
  syncH2OColumn <- function(frame, source, variable) {
    if (is.null(frame)) return(NULL)

    updateFrame <- tryCatch(
      h2o::as.h2o(source[, variable, drop = FALSE]),
      error = function(cond) {
        message(
          paste0(
            "Variable '", variable,
            "' could not be uploaded to the Java server.\n"
          )
        )
        stop(cond)
      }
    )

    frame <- tryCatch({
      frame[, variable] <- updateFrame[, 1]
      h2o::h2o.getId(frame)
      frame
    }, error = function(cond) {
      message(
        paste0(
          "Variable '", variable,
          "' could not be updated on the Java server.\n"
        )
      )
      stop(cond)
    })

    try(h2o::h2o.rm(updateFrame), silent = TRUE)

    frame
  }

  # Rebuild the H2O working frames from the last valid R-side state.
  # This is used after a failed variable-specific iteration so that a
  # retry never inherits a partially modified or invalid H2OFrame.
  rebuildH2OFrames <- function(data, bdata, hex = NULL, bhex = NULL) {

    if (!is.null(hex)) {
      try(h2o::h2o.rm(hex), silent = TRUE)
    }

    if (!is.null(bhex)) {
      try(h2o::h2o.rm(bhex), silent = TRUE)
    }

    new_hex <- h2o::as.h2o(data)
    new_bhex <- NULL

    if (!is.null(bdata)) {
      new_bhex <- h2o::as.h2o(bdata)
    }

    list(hex = new_hex, bhex = new_bhex)
  }

  # ------------------------------------------------------------
  # Generate H2O datasets if flushing is disabled
  # ============================================================
  if (!flush) {
    tryCatch(hex <- h2o::as.h2o(data),
             error = function(cond) {
               message("trying to upload data to JAVA server...\n")
               message("ERROR: Data could not be uploaded to the Java Server\nJava server returned the following error:\n")
               stop(cond)
             })

    bhex <- NULL
    if (!is.null(bdata)) {
      tryCatch(bhex <- h2o::as.h2o(bdata),
               error = function(cond) {
                 message("trying to upload bootstrap data to JAVA server...\n")
                 message("ERROR: Bootstrap data could not be uploaded to the Java Server\nJava server returned the following error:\n")
                 stop(cond)
               })
    }
  }
  else {
    hex <- NULL
    bhex <- NULL
  }

  # ============================================================
  # Global iteration loop
  # ============================================================
  while (running) {

    # ----------------------------------------------------------
    # Regenerate multilevel summary variables
    # ----------------------------------------------------------
    if (!is.null(hierarchy)) {

      # Remove summaries generated in the previous global iteration.
      old_multilevel <- attr(data, "mlim.multilevel.variables")
      if (!is.null(old_multilevel) && length(old_multilevel) > 0L) {
        data <- data[
          , setdiff(names(data), old_multilevel),
          drop = FALSE
        ]
      }

      attr(data, "mlim.hierarchy") <- NULL
      attr(data, "mlim.original.variables") <- NULL
      attr(data, "mlim.multilevel.variables") <- NULL

      data <- mlim.multilevel.R(
        data = data,
        hierarchy = hierarchy,
        variables = multilevel_source_variables
      )

      multilevel_variables <-
        attr(data, "mlim.multilevel.variables")

      # Multiple imputation uses a separate bootstrap working dataset.
      if (!is.null(bdata)) {

        old_b_multilevel <- attr(
          bdata,
          "mlim.multilevel.variables"
        )

        if (!is.null(old_b_multilevel) &&
            length(old_b_multilevel) > 0L) {
          bdata <- bdata[
            , setdiff(names(bdata), old_b_multilevel),
            drop = FALSE
          ]
        }

        attr(bdata, "mlim.hierarchy") <- NULL
        attr(bdata, "mlim.original.variables") <- NULL
        attr(bdata, "mlim.multilevel.variables") <- NULL

        bdata <- mlim.multilevel.R(
          data = bdata,
          hierarchy = hierarchy,
          variables = intersect(
            multilevel_source_variables,
            names(bdata)
          ),
          weights = bdata[["mlim_bootstrap_weights_column_"]]
        )
      }

      # Make the generated summaries available to every imputation model.
      allPredictors <- unique(
        c(basePredictors, multilevel_variables)
      )

      X <- unique(
        c(baseX, multilevel_variables)
      )

      # With flush = FALSE, keep the existing H2O frames and update
      # only the regenerated summary columns.
      if (!flush && length(multilevel_variables) > 0L) {

        for (v in multilevel_variables) {
          hex <- syncH2OColumn(hex, data, v)
        }

        if (!is.null(bdata)) {
          b_multilevel_variables <-
            attr(bdata, "mlim.multilevel.variables")

          for (v in b_multilevel_variables) {
            bhex <- syncH2OColumn(bhex, bdata, v)
          }
        }
      }
    }

    message(paste0("\ndata ", m.it, ", iteration ", k,
                   " (RAM = ", memuse::Sys.meminfo()$freeram, "):"))
    md.log(paste("Iteration", k), section = "subsection")

    # ----------------------------------------------------------
    # Variable-wise imputation loop
    # ----------------------------------------------------------
    for (Y in ITERATIONVARS[z:length(ITERATIONVARS)]) {
      start <- as.integer(Sys.time())

      # Progress bar and console text
      # --------------------------------------------------------
      if (verbose == 0) {
        pb <- txtProgressBar((which(ITERATIONVARS == Y)) - 1,
                             length(vars2impute), style = 3)
      }
      if (verbose != 0) message(paste0("    ", Y))

      # A variable-specific H2O/AutoML step may fail transiently.
      # Try the same imputation up to three times before skipping the
      # variable. The same seed is retained across attempts.
      max_attempts <- 3L
      it <- NULL
      last_error <- NULL

      for (attempt in seq_len(max_attempts)) {

        it <- NULL
        last_error <- NULL

        # After a failed attempt, recreate the H2O working frames from
        # the last successfully accepted R-side state before retrying.
        if (attempt > 1L && !flush) {
          frames <- tryCatch(
            rebuildH2OFrames(
              data = data,
              bdata = bdata,
              hex = hex,
              bhex = bhex
            ),
            error = function(cond) {
              last_error <<- cond
              NULL
            }
          )

          if (is.null(frames)) {
            if (attempt < max_attempts) {
              message(
                paste0(
                  "\nReimputing '", Y, "' failed on attempt ",
                  attempt, " of ", max_attempts,
                  " while rebuilding the H2O working data. Retrying..."
                )
              )

              if (!is.null(last_error)) {
                message(last_error)
              }

              Sys.sleep(sleep)
              next
            }

            break
          }

          hex <- frames$hex
          bhex <- frames$bhex

          if (debug) {
            md.log(
              paste(
                "retry", attempt, "of", max_attempts,
                "- H2O working data reuploaded"
              ),
              date = debug, time = debug, trace = FALSE
            )
          }
        }

        tryCatch(
          capture.output(
            it <- iterate(
              MI, dataNA, bdataNA,
              preimputed.data, data, bdata, boot, hex, bhex, metrics, tolerance,
              m, k, X, Y, z = which(ITERATIONVARS == Y), m.it,

              # loop data
              ITERATIONVARS, vars2impute,
              allPredictors, preimpute, impute,

              # settings
              error_metric, FAMILY = FAMILY, cv, tuning_time,
              max_models,
              autobalance,
              seed, save, flush,
              verbose, debug, report, sleep,

              # saving settings
              mem, orderedCols, ignore, maxiter,
              matching, ignore.rank,
              verbosity, error, cpu, max_ram, min_ram,
              stochastic
            ),
            file = report,
            append = TRUE
          ),
          error = function(cond) {
            last_error <<- cond
            it <<- NULL
          }
        )

        # Successful attempt
        if (!is.null(it)) {
          if (attempt > 1L) {
            message(
              paste0(
                "\nReimputing '", Y, "' succeeded on attempt ",
                attempt, " of ", max_attempts, "."
              )
            )
          }
          break
        }

        # Failed attempt, but another retry remains
        if (attempt < max_attempts) {
          message(
            paste0(
              "\nReimputing '", Y, "' failed on attempt ",
              attempt, " of ", max_attempts, ". Retrying..."
            )
          )

          if (!is.null(last_error)) {
            message(last_error)
          }

          if (debug) {
            md.log(
              paste(
                "Reimputing", Y, "failed on attempt",
                attempt, "of", max_attempts, "- retrying"
              ),
              date = TRUE, time = TRUE, print = FALSE, trace = FALSE
            )
          }

          Sys.sleep(sleep)
        }
      }

      # All three attempts failed. Restore clean H2O working frames before
      # moving to the next variable. If the frames themselves cannot be
      # restored, the H2O session is no longer usable and the error is fatal.
      if (is.null(it)) {

        if (!flush) {
          frames <- tryCatch(
            rebuildH2OFrames(
              data = data,
              bdata = bdata,
              hex = hex,
              bhex = bhex
            ),
            error = function(cond) {
              message(
                paste0(
                  "\nThe H2O working data could not be restored after ",
                  max_attempts, " failed attempts for '", Y, "'."
                )
              )
              stop(cond)
            }
          )

          hex <- frames$hex
          bhex <- frames$bhex
        }

        message(
          paste0(
            "\nReimputing '", Y, "' failed after ", max_attempts,
            " attempts and this variable will be skipped!\n",
            "See the last error below:"
          )
        )

        md.log(
          paste(
            "Reimputing", Y, "failed after", max_attempts,
            "attempts and the variable will be skipped!"
          ),
          date = TRUE, time = TRUE, print = TRUE, trace = FALSE
        )

        if (!is.null(last_error)) {
          message(last_error)
        }
      }

      # Update the working state returned by iterate()
      # --------------------------------------------------------
      if (!is.null(it)) {
        X             <- it$X
        ITERATIONVARS <- it$iterationvars
        metrics       <- it$metrics
        data          <- it$data
        bdata         <- it$bdata
        hex           <- it$hex
        bhex          <- it$bhex
      }

      # Log and status bar
      # --------------------------------------------------------
      time <- as.integer(Sys.time()) - start
      if (debug) {
        md.log(paste("done! after:", time, "seconds"),
               date = debug, time = debug, print = FALSE, trace = FALSE)
      }

      if (verbose == 0) {
        setTxtProgressBar(pb, which(ITERATIONVARS == Y))
      }
    }

    # ----------------------------------------------------------
    # Evaluate the stopping criterion
    #
    # Variable-specific acceptance is handled inside iterate().
    # Continue only when at least one variable improved during the
    # current global iteration and maxiter has not been reached.
    # ----------------------------------------------------------
    if (debug) {
      md.log("evaluating stopping criteria",
             date = debug, time = debug, trace = FALSE)
    }

    SC <- stoppingCriteria(
      metrics = metrics,
      k = k,
      maxiter = maxiter,
      error_metric = error_metric
    )

    if (debug) {
      md.log(paste("running:", SC$running),
             date = debug, time = debug, trace = FALSE)
      md.log(paste("\nEstimated", error_metric, "error:", SC$error),
             section = "paragraph", date = debug, time = debug, trace = FALSE)
    }

    running <- SC$running
    error <- SC$error

    # Start subsequent global iterations from the first variable.
    z <- 1L
    k <- k + 1L
  }

  # ------------------------------------------------------------
  # End of the iterations
  # ------------------------------------------------------------
  if (verbose) message("\n")
  md.log("", section = "paragraph", trace = FALSE)

  # Always return the final working data.
  if (clean) {
    tryCatch(h2o::h2o.removeAll(),
             error = function(cond) {
               message("trying to connect to JAVA server...\n")
               stop("Java server has crashed (low RAM?)")
             })
    md.log("server was cleaned", section = "paragraph", trace = FALSE)
  }

  if (shutdown) {
    md.log("shutting down the server", section = "paragraph", trace = FALSE)
    tryCatch(h2o::h2o.shutdown(prompt = FALSE),
             error = function(cond) {
               message("trying to connect to JAVA server...\n")
               warning("Java server has crashed (low RAM?)")
             })
    Sys.sleep(sleep)
  }

  # ------------------------------------------------------------
  # Match ordered variables to their valid ordinal support
  # ============================================================
  if (matching == "AUTO" && !ignore.rank &&
      length(orderedCols) > 0L) {

    orderedNames <- colnames(data)[orderedCols]

    for (i in seq_along(orderedNames)) {
      Y <- orderedNames[i]

      if (Y %in% vars2impute) {
        v.na <- dataNA[, Y]
        support <- mem[[i]][[1]]$support

        if (debug) {
          md.log(paste("matching", Y),
                 section = "paragraph")
        }

        data[v.na, Y] <- matching(
          imputed = data[v.na, Y],
          support = support
        )
      }
    }
  }

  # ------------------------------------------------------------
  # Revert ordinal transformation
  # ============================================================
  if (!ignore.rank) {
    data[, orderedCols] <- revert(data[, orderedCols, drop = FALSE], mem)
  }

  # Remove derived multilevel predictors before returning the completed data.
  if (!is.null(hierarchy)) {
    multilevel_variables <- attr(
      data,
      "mlim.multilevel.variables"
    )

    if (!is.null(multilevel_variables) &&
        length(multilevel_variables) > 0L) {
      data <- data[
        , setdiff(names(data), multilevel_variables),
        drop = FALSE
      ]
    }

    attr(data, "mlim.hierarchy") <- NULL
    attr(data, "mlim.original.variables") <- NULL
    attr(data, "mlim.multilevel.variables") <- NULL
  }

  attr(data, "metrics") <- metrics
  attr(data, error_metric) <- error

  class(data) <- c("mlim", "data.frame")
  return(dataLast = data)
}
