#' @title iteration_loop
#' @description runs imputation iteration loop to fully impute a dataframe
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom h2o h2o.init as.h2o h2o.predict h2o.ls
#'             h2o.removeAll h2o.rm h2o.shutdown h2o.getId
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

iteration_loop <- function(MI, dataNA, preimputed.data, data, bdata, boot, metrics, tolerance,
                           m, k, X, Y, z, m.it,

                           # loop data
                           vars2impute,
                           allPredictors, preimpute, impute,
                           hierarchy = NULL,

                           # settings
                           error_metric, FAMILY, cv, tuning_time,
                           max_models,
                           keep_cv, #should be removed
                           autobalance, #balance,
                           seed, save, flush,
                           verbose, debug, report, sleep,

                           # saving settings
                           mem, orderedCols, ignore, maxiter,
                           miniter, matching, ignore.rank,
                           verbosity, error, cpu, max_ram, min_ram, shutdown, clean,
                           stochastic, connection, port, insecure, https,
                           bind_to_localhost, ignore_config, java,
                           running = TRUE) {

  # ------------------------------------------------------------
  # bootrtap
  #
  # Bootstrap from the original dataset, hold the original NA values,
  # and then use the preimputed dataset, and then gradually improve it
  #
  #### PROBLEM
  ############
  #### drop the duplicates because they screw up the k-fold cross-validation.
  #### multiple identical observations might go to train and test datasets.
  #### here I suggest several 'work-in-progress' solutions
  # ============================================================

  # Bootstrap duplicates are represented through observation weights.
  # When autobalance is requested, iterate() combines these multiplicity
  # weights with outcome-balancing weights for multiple imputation.

  if (boot) {
    rownames(data) <- 1:nrow(data) #remember the rows that are missing
    sampling_index <- sample(x = nrow(data), size = nrow(data), replace=TRUE)



    ## SOLUTION 1: DROP THE DUPLICATES AND DO UNDERSAMPLING
    ## ----------------------------------------------------
    # bdata <- data[sampling_index, ]
    # bdataNA <- is.na(bdata[, vars2impute, drop = FALSE])
    # bdata <- mlim.preimpute(data=bdata, preimpute=preimpute, seed = NULL)
    # sampling_index <- sampling_index[!duplicated(sampling_index)]
    # bdata <- data[sampling_index, ]
    # bdata[, "mlim_bootstrap_weights_column_"] <- 1
    # bdataNA <- is.na(bdata[, vars2impute, drop = FALSE])

    ## SOLUTION 2: ADD THE DUPLICATES TO THE WEIGHT_COLUMN
    ## ----------------------------------------------------
    dups <- bootstrapWeight(sampling_index)
    bdata <- data[1:nrow(data) %in% dups[,1], ]
    bdataNA <- is.na(bdata[, vars2impute, drop = FALSE])
    message("\n")
    bdata <- mlim.preimpute(data=bdata, preimpute=preimpute, seed = NULL)
    bdata[, "mlim_bootstrap_weights_column_"] <- dups[,2] #OR ALTERNATIVELY #dups[,2] / sum(dups[,2])

    ## SOLUTION 3: Assign CV folding manually instead of weight_column
    ## ----------------------------------------------------
    # bdata <- data[sampling_index, ]
    # bdataNA <- is.na(bdata[, vars2impute, drop = FALSE])
    # bdata <- mlim.preimpute(data=bdata, preimpute=preimpute, seed = NULL)
    # bdata[, "mlim_bootstrap_fold_assignment_"] <- 0
    # folds <- bootstrapCV(index = sampling_index, cv = cv)
    # for (i in 1:cv) {
    #   indexcv <- sampling_index %in% folds[,i]
    #   bdata[indexcv, "mlim_bootstrap_fold_assignment_"] <- i
    # }
  }

  # update the fresh data
  # ------------------------------------------------------------

  if (debug) md.log("data was sent to h2o cloud", date=debug, time=debug, trace=FALSE)

  # define iteration var. this is a vector of varnames that should be imputed
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

    tryCatch(
      h2o::h2o.rm(updateFrame),
      error = function(cond) {
        message(
          paste0(
            "H2O cleanup failed while removing the temporary update frame for variable '",
            variable, "'.\n",
            "Error: ", conditionMessage(cond)
          )
        )
        NULL
      }
    )

    frame
  }

  # ------------------------------------------------------------
  # Generate the HEX datasets if there is NO FLUSHING
  # ------------------------------------------------------------
  if (!flush) {
    tryCatch(hex <- h2o::as.h2o(data),
             error = function(cond) {
               message("trying to upload data to JAVA server...\n");
               message("ERROR: Data could not be uploaded to the Java Server\nJava server returned the following error:\n")
               return(stop(cond))})

    bhex <- NULL
    if (!is.null(bdata)) {
      tryCatch(bhex<- h2o::as.h2o(bdata),
               error = function(cond) {
                 message("trying to upload data to JAVA server...\n");
                 message("ERROR: Data could not be uploaded to the Java Server\nJava server returned the following error:\n")
                 return(stop(cond))})
    }
  }
  else {
    hex  <- NULL
    bhex <- NULL
  }

  # ============================================================
  # ============================================================
  # global iteration loop
  # ============================================================
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

      data <- mlim.multilevel(
        data = data,
        hierarchy = hierarchy,
        variables = multilevel_source_variables
      )

      multilevel_variables <- attr(data, "mlim.multilevel.variables")

      # Multiple imputation uses a separate bootstrap working dataset.
      if (!is.null(bdata)) {

        old_b_multilevel <- attr(bdata, "mlim.multilevel.variables")

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

        bdata <- mlim.multilevel(
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

      X <- unique(c(baseX, multilevel_variables))

      # With flush = FALSE, keep the existing H2O frames and update
      # only the regenerated summary columns.
      if (!flush && length(multilevel_variables) > 0L) {

        for (v in multilevel_variables) {
          hex <- syncH2OColumn(hex, data, v)
        }

        if (!is.null(bdata)) { b_multilevel_variables <- attr(bdata, "mlim.multilevel.variables")

        for (v in b_multilevel_variables) {
          bhex <- syncH2OColumn(bhex, bdata, v)
        }
        }
      }
    }

    # always print the iteration
    message(paste0("\ndata ", m.it, ", iteration ", k, " (RAM = ", memuse::Sys.meminfo()$freeram,")", ":"), sep = "") #":\t"
    md.log(paste("Iteration", k), section="subsection")

    # ## AVOID THIS PRACTICE BECAUSE DOWNLOADING DATA FROM THE SERVER IS SLOW
    # # store the last data
    # if (debug) md.log("store last data", date=debug, time=debug, trace=FALSE)
    # dataLast <- as.data.frame(hex)
    # attr(dataLast, "metrics") <- metrics
    # attr(dataLast, "rmse") <- error

    for (Y in ITERATIONVARS[z:length(ITERATIONVARS)]) {
      start <- as.integer(Sys.time())

      # Prepare the progress bar and iteration console text
      # ============================================================
      if (verbose==0) pb <- txtProgressBar((which(ITERATIONVARS == Y))-1, length(vars2impute), style = 3)
      if (verbose!=0) message(paste0("    ",Y))

      it <- NULL
      tryCatch(capture.output(
        it <- iterate(
          MI, dataNA, bdataNA,
          preimputed.data, data, bdata, boot, hex, bhex, metrics, tolerance,
          m, k, X, Y, z=which(ITERATIONVARS == Y), m.it,
          # loop data
          ITERATIONVARS, vars2impute,
          allPredictors, preimpute, impute,
          hierarchy = hierarchy,
          # settings
          error_metric, FAMILY=FAMILY, cv, tuning_time,
          max_models,
          keep_cv,
          autobalance, #balance,
          seed, save, flush,
          verbose, debug, report, sleep,
          # saving settings
          mem, orderedCols, ignore, maxiter,
          miniter, matching, ignore.rank,
          verbosity, error, cpu, max_ram, min_ram, stochastic,
          port, insecure, https, bind_to_localhost, ignore_config, java)
        , file = report, append = TRUE)
        , error = function(cond) {
          message(paste0("\nReimputing '", Y, "' with the current specified algorithms failed and this variable will be skipped! \nSee Java server's error below:"));
          md.log(paste("Reimputing", Y, "failed and the variable will be skipped!"),
                 date = TRUE, time = TRUE, print = TRUE)
          message(cond)

          ### ??? activate the code below if you allow "iterate" preimputation
          ### ??? or should it be ignored...
          # if (preimpute == "iterate" && k == 1L && (Y %in% allPredictors)) {
          #   X <- union(X, Y)
          #   if (debug) md.log("x was updated", date=debug, time=debug, trace=FALSE)
          # }
          return(NULL)
        })



      # If there was no error, update the variables
      # else make sure the model is cleared
      # --------------------------------------------------------------
      #IT <<- it
      if (!is.null(it)) {
        X             <- it$X
        ITERATIONVARS <- it$iterationvars
        metrics       <- it$metrics
        data          <- it$data
        bdata         <- it$bdata
        hex           <- it$hex
        bhex          <- it$bhex

      }

      # Check the H2O server after each variable-specific iteration.
      # --------------------------------------------------------------
      if (!server.check(connection)) {
        message("H2O server is unavailable. Restarting the server...\n")
        connection <- NULL
        try(stopH2o(port = port), silent = TRUE)
        Sys.sleep(0.25)

        capture.output(
          connection <- init(nthreads = cpu,
                             min_mem_size = min_ram,
                             max_mem_size = max_ram,
                             ignore_config = ignore_config,
                             java = java,
                             report,
                             debug,
                             port = port,
                             insecure = insecure,
                             https = https,
                             bind_to_localhost = bind_to_localhost),
          file = report, append = TRUE
        )

        if (!flush) {
          hex <- h2o::as.h2o(data)
          bhex <- if (is.null(bdata)) NULL else h2o::as.h2o(bdata)
        }
      }

      # log & statusbar
      # --------------------------------------------------------------
      time = as.integer(Sys.time()) - start
      if (debug) md.log(paste("done! after: ", time, " seconds"),
                        date = TRUE, time = TRUE, print = FALSE, trace = FALSE)

      # update the statusbar
      if (verbose==0) setTxtProgressBar(pb, (which(ITERATIONVARS == Y)))
    }

    # CHECK CRITERIA FOR RUNNING THE NEXT ITERATION
    # --------------------------------------------------------------
    if (debug) md.log("evaluating stopping criteria", date=debug, time=debug, trace=FALSE)
    SC <- stoppingCriteria(method="varwise_NA", miniter, maxiter,
                           metrics, k, vars2impute,
                           error_metric,
                           tolerance,
                           md.log = report)
    if (debug) {
      md.log(paste("running: ", SC$running), date=debug, time=debug, trace=FALSE)
      md.log(paste("\nEstimated", error_metric, "error:", SC$error), section="paragraph", date=debug, time=debug, trace=FALSE)
    }

    running <- SC$running
    error <- SC$error

    # update the loop number
    k <- k + 1L
  }

  # ............................................................
  # END OF THE ITERATIONS
  # ............................................................
  if (verbose) message("\n")
  md.log("", section="paragraph", trace=FALSE)

  # # if the iterations stops on minimum or maximum, return the last data
  # if (k == miniter || (k == maxiter && running) || maxiter == 1) {
  ###### ALWAYS RETURN THE LAST DATA. THIS WAS A BUG, REMAINING AFTER I INDIVIDUALIZED IMPUTATION EVALUATION

  ### Workaround for buggy 'as.data.frame' function
  ### =============================================

  # INSTEAD OF DEFINING A NEW VARIABLE 'dataLast', just use the 'data' returned
  # FROM iteration and most importantly, AVOID THE BLOODY 'as.data.frame' function
  # which IS SO BUGGY
  # dataLast <- as.data.frame(hex)
  # Sys.sleep(sleep)
  # attr(dataLast, "metrics") <- metrics
  # attr(dataLast, error_metric) <- error
  # }
  # else {
  #   md.log("return previous iteration's data", date=debug, time=debug, trace=FALSE)
  # }

  if (clean) {
    tryCatch(h2o::h2o.removeAll(),
             error = function(cond) {
               message("trying to connect to JAVA server...\n");
               return(stop("Java server has crashed (low RAM?)"))})
    md.log("server was cleaned", section="paragraph", trace=FALSE)
  }

  if (shutdown) {
    md.log("shutting down the server", section="paragraph", trace=FALSE)
    tryCatch(h2o::h2o.shutdown(prompt = FALSE),
             error = function(cond) {
               message("trying to connect to JAVA server...\n");
               return(warning("Java server has crashed (low RAM?)"))})
    Sys.sleep(sleep)
  }

  # ------------------------------------------------------------
  # Auto-Matching specifications
  # ============================================================
  if (matching == "AUTO") {
    mtc <- 0
    for (Y in vars2impute) {
      mtc <- mtc + 1
      v.na <- dataNA[, Y]

      if ((FAMILY[mtc] == 'gaussian_integer') | (FAMILY[mtc] == 'quasibinomial')) {
        if (debug) md.log(paste("matching", Y), section="paragraph")

        matchedVal <- matching(imputed=data[v.na, Y],
                               nonMiss=unique(data[!v.na,Y]),
                               md.log)
        #message(matchedVal)
        if (!is.null(matchedVal)) data[v.na, Y] <- matchedVal
        else {
          md.log("matching failed", section="paragraph", trace=FALSE)
        }
      }
    }
  }

  # ------------------------------------------------------------
  # Revert ordinal transformation
  # ============================================================
  if (!ignore.rank) {
    data[, orderedCols] <-  revert(data[, orderedCols, drop = FALSE], mem)
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
  return(dataLast=data)
}
