#' @title iterate
#' @description runs imputation iterations for different settings, both single
#'              imputation and multiple imputation.in addition, it can do iterations
#'              for both "imputation" and "postimputation". postimputation begins if
#'              a powerful algorithm - that requires a lot of time for fine-tuning -
#'              is specified for the imputation. such algorithms are used last in the imputation
#'              to save time.
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.predict h2o.ls h2o.getId
#'             h2o.removeAll h2o.rm h2o.shutdown h2o.load_frame h2o.save_frame
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit rnorm
#' @importFrom missRanger imputeUnivariate
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd


iterate <- function(MI, dataNA, bdataNA,
                    preimputed.data, data, bdata, boot, hex, bhex, metrics, tolerance,
                    m, k, X, Y, z, m.it,

                    # loop data
                    ITERATIONVARS, vars2impute,
                    allPredictors, preimpute, impute,

                    # settings
                    error_metric, FAMILY, cv, tuning_time,
                    max_models,
                    autobalance,
                    seed, save, flush,
                    verbose, debug, report, sleep,

                    # saving settings
                    mem, orderedCols, ignore, maxiter,
                    matching, ignore.rank,
                    verbosity, error, cpu, max_ram, min_ram,
                    stochastic
) {

  # Update the report
  # ============================================================
  if (debug) {
    md.log(paste0("\n", Y), section = "paragraph")
    md.log(paste("family:", FAMILY[z]), trace = FALSE)
  } else {
    md.log(paste("imputing:", Y, " family:", FAMILY[z],
                 "(RAM =", memuse::Sys.meminfo()$freeram, ")"),
           trace = FALSE)
  }

  # Redefine HEX and BHEX, if flushing is activated
  # ============================================================
  if (flush) {
    tryCatch(hex <- h2o::as.h2o(data),
             error = function(cond) {
               message("Data could not be uploaded to Java server. see the error below:\n")
               stop(cond)
             })

    if (!is.null(bdata)) {
      tryCatch(bhex <- h2o::as.h2o(bdata),
               error = function(cond) {
                 message("Bootstrap data could not be uploaded to Java server. see the error below:\n")
                 stop(cond)
               })
    }

    if (debug) md.log("data reuploaded", date = debug, time = debug, trace = FALSE)
  }

  # Index the missing data
  # ============================================================
  v.na <- dataNA[, Y]
  if (boot) b.na <- bdataNA[, Y]

  # Generate deterministic or stochastic imputations from predictions
  # ============================================================
  imputedValues <- function(pred, family, rmse) {

    # continuous / integer-like outcomes
    if (family %in% c("gaussian", "gaussian_integer", "quasibinomial")) {
      values <- as.vector(pred[, 1])

      if (stochastic) {
        rmse <- as.numeric(rmse)[1]
        if (!is.finite(rmse) || rmse < 0) {
          stop(paste0("A valid RMSE is required for stochastic imputation of '", Y, "'."))
        }

        values <- stats::rnorm(
          n = length(values),
          mean = values,
          sd = rmse
        )
      }

      return(values)
    }

    # categorical outcomes
    if (family %in% c("binomial", "multinomial")) {
      if (!stochastic) return(as.vector(pred[, 1]))

      predDF <- tryCatch(as.data.frame(pred),
                         error = function(cond) {
                           message("Predicted probabilities could not be converted to a data.frame.\n")
                           stop(cond)
                         })

      if (ncol(predDF) < 2L) {
        stop(paste0("Predicted class probabilities are unavailable for stochastic imputation of '", Y, "'."))
      }

      probMat <- predDF[, -1, drop = FALSE]
      return(stochasticFactorImpute(
        levels = colnames(probMat),
        probMat = as.matrix(probMat)
      ))
    }

    stop(paste(family, "is not recognized"))
  }

  # Keep the H2O working frame synchronized with the R data.frame
  # ============================================================
  syncH2OColumn <- function(frame, source, variable) {
    if (is.null(frame)) return(NULL)

    updateFrame <- tryCatch(h2o::as.h2o(source[, variable, drop = FALSE]),
                            error = function(cond) {
                              message(paste0("Variable '", variable,
                                             "' could not be uploaded to the Java server.\n"))
                              stop(cond)
                            })

    frame <- tryCatch({
      frame[, variable] <- updateFrame[, 1]

      # H2OFrame replacement is lazy. Force the updated frame to be
      # evaluated before removing the temporary source column.
      h2o::h2o.getId(frame)
      frame
    }, error = function(cond) {
      message(paste0("Variable '", variable,
                     "' could not be updated on the Java server.\n"))
      stop(cond)
    })

    # The temporary source frame is safe to remove only after the
    # replacement expression above has been evaluated.
    try(h2o::h2o.rm(updateFrame), silent = TRUE)

    return(frame)
  }

  # Remove H2O objects created by the current AutoML run
  # ============================================================
  cleanupAutoML <- function(fit = NULL, pred = NULL, bpred = NULL) {

    # Prediction frames are no longer needed once values have been copied to R.
    if (!is.null(pred)) try(h2o::h2o.rm(pred), silent = TRUE)
    if (!is.null(bpred)) try(h2o::h2o.rm(bpred), silent = TRUE)

    if (!is.null(fit)) {
      # Remove every model created by this AutoML run, including dependencies
      # such as cross-validation submodels and stacked-ensemble components.
      model_ids <- tryCatch(
        as.data.frame(fit@leaderboard)$model_id,
        error = function(cond) character(0)
      )

      if (length(model_ids) > 0L) {
        try(h2o::h2o.rm(model_ids, cascade = TRUE), silent = TRUE)
      }

      # Remove AutoML metadata frames after their model IDs have been extracted.
      try(h2o::h2o.rm(fit@leaderboard), silent = TRUE)
      try(h2o::h2o.rm(fit@event_log), silent = TRUE)
    }

    invisible(NULL)
  }

  # If predictors are NULL, randomly fill the missing values
  # ============================================================
  if (length(X) == 0L) {
    if (debug) md.log("uni impute", date = debug, time = debug, trace = FALSE)

    data[[Y]] <- missRanger::imputeUnivariate(data[[Y]])
    if (boot && !is.null(bdata)) {
      bdata[[Y]] <- missRanger::imputeUnivariate(bdata[[Y]])
    }

    if (!flush) {
      hex <- syncH2OColumn(hex, data, Y)
      if (boot && !is.null(bdata)) bhex <- syncH2OColumn(bhex, bdata, Y)
    }
  } else {
    if (debug) md.log(paste("X:", paste(setdiff(X, Y), collapse = ", ")),
                      date = debug, time = debug, trace = FALSE)
    if (debug) md.log(paste("Y:", Y),
                      date = debug, time = debug, trace = FALSE)

    sort_metric <- "AUTO"
    fit <- NULL
    pred <- NULL
    bpred <- NULL

    # If an error interrupts this variable-specific step, remove any H2O
    # objects that were already created by the current AutoML run.
    on.exit({
      if (!flush) cleanupAutoML(fit = fit, pred = pred, bpred = bpred)
    }, add = TRUE)

    # Each variable-specific model is a separate AutoML problem. The
    # working H2O frame changes as imputations are updated, so reusing a
    # single AutoML project name across variables/iterations is invalid.
    project_name <- paste0(
      "mlim_", m.it, "_", k, "_", z, "_",
      gsub("[^A-Za-z0-9_]", "_", Y)
    )

    # Prepare bootstrap and balancing weights
    # ============================================================
    weights_column <- if (is.null(bhex)) {
      NULL
    } else {
      "mlim_bootstrap_weights_column_"
    }

    ordered_target <- !ignore.rank &&
      length(orderedCols) > 0L &&
      Y %in% colnames(data)[orderedCols]

    categorical_target <- FAMILY[z] %in% c(
      "binomial", "multinomial"
    )

    if (boot && isTRUE(autobalance) &&
        (categorical_target || ordered_target)) {

      observed <- !b.na
      bootstrap_weight <-
        bdata[["mlim_bootstrap_weights_column_"]]
      target <- as.character(bdata[[Y]][observed])

      weighted_count <- tapply(
        bootstrap_weight[observed],
        target,
        sum
      )

      balance_weight <- sum(weighted_count) /
        (length(weighted_count) * weighted_count)

      model_weight <- bootstrap_weight
      model_weight[observed] <-
        bootstrap_weight[observed] *
        as.numeric(balance_weight[target])

      bdata[["mlim_model_weights_column_"]] <-
        model_weight

      bhex <- syncH2OColumn(
        bhex, bdata,
        "mlim_model_weights_column_"
      )

      weights_column <-
        "mlim_model_weights_column_"
    }

    # Fine-tune a regression model
    # ============================================================
    if (FAMILY[z] %in% c("gaussian", "gaussian_integer", "quasibinomial")) {
      tryCatch(
        fit <- h2o::h2o.automl(
          x = setdiff(X, Y),
          y = Y,
          training_frame = if (is.null(bhex)) hex[which(!v.na), ] else bhex[which(!b.na), ],
          sort_metric = sort_metric,
          project_name = project_name,
          include_algos = impute,
          nfolds = cv,
          exploitation_ratio = 0.1,
          max_runtime_secs = tuning_time,
          max_models = max_models,
          weights_column = weights_column,
          keep_cross_validation_predictions = FALSE,
          keep_cross_validation_models = FALSE,
          keep_cross_validation_fold_assignment = FALSE,
          seed = seed
        ),
        error = function(cond) {
          message(paste("\nModel training for variable", Y,
                        "failed... see the Java server error below:\n"))
          stop(cond)
        }
      )
    } else if (FAMILY[z] %in% c("binomial", "multinomial")) {

      # Fine-tune a classification model
      # ============================================================
      # For single imputation, use H2O class balancing. In multiple
      # imputation, balancing is represented through target-specific
      # observation weights combined with bootstrap multiplicities.
      balance_classes <- isTRUE(autobalance) && !boot

      tryCatch(
        fit <- h2o::h2o.automl(
          x = setdiff(X, Y),
          y = Y,
          balance_classes = balance_classes,
          sort_metric = sort_metric,
          training_frame = if (is.null(bhex)) hex[which(!v.na), ] else bhex[which(!b.na), ],
          project_name = project_name,
          include_algos = impute,
          nfolds = cv,
          exploitation_ratio = 0.1,
          max_runtime_secs = tuning_time,
          max_models = max_models,
          weights_column = weights_column,
          keep_cross_validation_predictions = FALSE,
          keep_cross_validation_models = FALSE,
          keep_cross_validation_fold_assignment = FALSE,
          seed = seed
        ),
        error = function(cond) {
          message("\nModel training failed... see the Java server error below:\n")
          stop(cond)
        }
      )
    } else {
      stop(paste(FAMILY[z], "is not recognized"))
    }

    Sys.sleep(sleep)
    if (debug) md.log("model fitted", date = debug, time = debug, trace = FALSE)
    if (debug) md.log(paste("leader:", fit@leader@model_id),
                      date = debug, time = debug, trace = FALSE)

    # Evaluate the retained model using cross-validation
    # ============================================================
    perf <- tryCatch(h2o::h2o.performance(fit@leader, xval = TRUE),
                     error = function(cond) {
                       message("\nModel performance evaluation failed...\nSee Java server's error:\n")
                       stop(cond)
                     })

    Sys.sleep(sleep)
    iterationMetric <- extractMetrics(k = k, v = Y, perf = perf)

    # Decide whether the current model should update the imputations
    # ============================================================
    accepted <- TRUE

    if (k > 1L) {
      errPrevious <- min(metrics[metrics$variable == Y, error_metric], na.rm = TRUE)
      checkMetric <- iterationMetric[iterationMetric$variable == Y, error_metric]

      percentImprove <- (checkMetric - errPrevious) / errPrevious
      accepted <- is.finite(percentImprove) && percentImprove < -tolerance

      if (debug && accepted) {
        md.log("imputation was improved, new values are replaced",
               date = debug, time = debug, trace = FALSE)
        md.log(paste(round(percentImprove, 6), "<", -tolerance),
               date = debug, time = debug, trace = FALSE)
      } else if (debug && !accepted) {
        md.log("imputation was NOT improved, new values are rejected",
               date = debug, time = debug, trace = FALSE)
        md.log(paste(round(percentImprove, 6), ">=", -tolerance),
               date = debug, time = debug, trace = FALSE)
      }
    }

    # Generate new imputations only when the model is accepted
    # ============================================================
    if (accepted) {

      # Original data
      # ------------------------------------------------------------
      pred <- tryCatch(h2o::h2o.predict(fit@leader,
                                        newdata = hex[which(v.na), X]),
                       error = function(cond) {
                         message("\nGenerating missing-data predictions failed...\nSee Java server's error:\n")
                         stop(cond)
                       })

      values <- imputedValues(
        pred = pred,
        family = FAMILY[z],
        rmse = iterationMetric[, "RMSE"]
      )

      tryCatch(data[which(v.na), Y] <- values,
               error = function(cond) {
                 message("\nThe imputed data could not be updated.\n")
                 stop(cond)
               })

      # The stochastic values must enter the H2O working frame so that
      # the next variable is conditioned on them.
      if (!flush) {
        hex <- syncH2OColumn(hex, data, Y)
      }

      # Bootstrap data used for multiple imputation
      # ------------------------------------------------------------
      if (boot) {
        bpred <- tryCatch(h2o::h2o.predict(fit@leader,
                                           newdata = bhex[which(b.na), X]),
                          error = function(cond) {
                            message("\nGenerating bootstrap-data predictions failed...\nSee Java server's error:\n")
                            stop(cond)
                          })

        bvalues <- imputedValues(
          pred = bpred,
          family = FAMILY[z],
          rmse = iterationMetric[, "RMSE"]
        )

        tryCatch(bdata[which(b.na), Y] <- bvalues,
                 error = function(cond) {
                   message("\nThe bootstrap imputed data could not be updated.\n")
                   stop(cond)
                 })

        if (!flush) {
          bhex <- syncH2OColumn(bhex, bdata, Y)
        }
      }
    } else {
      # NA signals that this variable did not improve in the current iteration.
      iterationMetric[, error_metric] <- NA
    }

    # Store the iteration metric
    # ============================================================
    metrics <- rbind(metrics, iterationMetric)

    # Remove all H2O objects created by the current AutoML run
    # ============================================================
    if (!flush) {
      cleanupAutoML(fit = fit, pred = pred, bpred = bpred)
      fit <- NULL
      pred <- NULL
      bpred <- NULL
    }

    gc()
  }

  if (debug) md.log("iteration done", date = debug, time = debug, trace = FALSE)

  # Save the current imputation state
  # ============================================================
  if (!is.null(save)) {
    if (debug) md.log("Saving the status", date = debug, time = debug, trace = FALSE)

    savestate <- list(

      # Data
      MI = MI,
      dataNA = dataNA,
      preimputed.data = preimputed.data,
      data = data,
      metrics = metrics,
      mem = mem,
      orderedCols = orderedCols,

      # Loop data
      m = m,
      k = k,
      z = z,
      X = X,
      Y = Y,
      m.it = m.it,
      vars2impute = vars2impute,
      FAMILY = FAMILY,
      ITERATIONVARS = ITERATIONVARS,
      allPredictors = allPredictors,

      # Settings
      preimpute = preimpute,
      impute = impute,
      ignore = ignore,
      autobalance = autobalance,
      save = save,
      maxiter = maxiter,
      cv = cv,
      tuning_time = tuning_time,
      max_models = max_models,
      matching = matching,
      ignore.rank = ignore.rank,
      stochastic = stochastic,
      seed = seed,
      verbosity = verbosity,
      verbose = verbose,
      debug = debug,
      report = report,
      flush = flush,
      error_metric = error_metric,
      tolerance = tolerance,
      error = error,
      cpu = cpu,
      max_ram = max_ram,
      min_ram = min_ram,

      # Package version used for the imputation
      pkg = packageVersion("mlim")
    )

    class(savestate) <- "mlim"
    saveRDS(savestate, save)
  }

  if (debug) md.log("saving done!", date = debug, time = debug, trace = FALSE)

  # Flush the Java server to regain RAM
  # ============================================================
  if (flush) {
    if (debug) md.log("flushing the server...", date = debug, time = debug, trace = FALSE)

    tryCatch(h2o::h2o.removeAll(),
             error = function(cond) {
               stop("Java server crashed. perhaps a RAM problem?")
             })

    Sys.sleep(sleep)

    # Clear R-side references after the H2O cluster has been emptied.
    gc()

    if (debug) md.log("server flushed", date = debug, time = debug, trace = FALSE)
    hex <- NULL
    bhex <- NULL
  }

  if (debug) md.log("flushing done! return to the loop",
                    date = debug, time = debug, trace = FALSE)

  return(list(
    X = X,
    metrics = metrics,
    iterationvars = ITERATIONVARS,
    hex = hex,
    bhex = bhex,
    data = data,
    bdata = bdata
  ))
}
