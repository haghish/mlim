#' @title iterate
#' @description runs imputation iterations for different settings, both single
#'              imputation and multiple imputation.in addition, it can do iterations
#'              for both "imputation" and "postimputation". postimputation begins if
#'              a powerful algorithm - that requires a lot of time for fine-tuning -
#'              is specified for the imputation. such algorithms are used last in the imputation
#'              to save time.
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.predict h2o.ls
#'             h2o.removeAll h2o.rm h2o.shutdown
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

iterate <- function(procedure,
                    MI, dataNA, data, bdata, boot, hex, bhex, metrics, tolerance, doublecheck,
                    m, k, X, Y, z, m.it,

                    # loop data
                    ITERATIONVARS, vars2impute,
                    allPredictors, preimpute, impute, postimpute,

                    # settings
                    error_metric, FAMILY, cv, tuning_time,
                    max_models, weights_column,
                    keep_cv,
                    autobalance, balance, seed, save, flush,

                    verbose, debug, report, sleep,

                    # saving settings
                    dataLast, mem, orderedCols, ignore, maxiter,
                    miniter, matching, ignore.rank,
                    verbosity, error, cpu, max_ram, min_ram
                    ) {

  # for the first dataframe imputation
  #if (is.null(bhex)) bhex <- hex

  #ZZ <- z
  #print(paste("z:",z, "length:",  length(vars2impute)))
  #if (z >= length(vars2impute)) ZZ <- length(vars2impute) - 1

  if (verbose==0) pb <- txtProgressBar(z-1, length(vars2impute), style = 3)
  if (debug) cat(paste0(Y," (RAM = ", memuse::Sys.meminfo()$freeram,")\n"))

  if (!boot) {
    v.na <- dataNA[, Y]
    y.na <- v.na
  }
  else {
    v.na <- dataNA[, Y]
    y.na <- rownames(bdata) %in% which(v.na)
  }

  if (debug) {
    md.log(Y, section="subsection")
    md.log(paste("family:", FAMILY[z]))
  }

  md.log(paste(X, collapse = ","))

  if (length(X) == 0L) {
    if (debug) md.log("uni impute", trace=FALSE)
    #??? change this with a self-written function
    tryCatch(hex[[Y]] <- h2o::as.h2o(missRanger::imputeUnivariate(data[[Y]])),
             error = function(cond) {
               #cat("connection to JAVA server failed...\n");
               return(NULL)})
    Sys.sleep(sleep)
  }
  else {

    if (debug) md.log(paste("X:",paste(setdiff(X, Y), collapse = ", ")), trace=FALSE)
    if (debug) md.log(paste("Y:",paste(Y, collapse = ", ")), trace=FALSE)

    # sort_metric specifications
    # ============================================================
    sort_metric <- "AUTO"
    #if (FAMILY[z] == 'binomial') {
    #  # check if Y is imbalanced
    #  if (is.imbalanced(data[[Y]])) sort_metric <- "AUCPR"
    #  else sort_metric <- "AUC"
    #}
    #else {
    #  sort_metric <- "AUTO"
    #}

    # specify "imputation" or "postimputation" procedure
    # ============================================================
    usedalgorithms <- NULL
    if (procedure == "impute") usedalgorithms <- impute
    else usedalgorithms <- postimpute
    # if (debug) print(paste("usedalgorithms", usedalgorithms))
    # cat("X:", X, "\n")
    # cat("Y:", Y, "\n")
    # cat("sort_metric:", sort_metric, "\n")
    # cat("usedalgorithms:", usedalgorithms, "\n")
    # cat("cv:", cv, "\n")
    # cat("tuning_time:", tuning_time, "\n")
    # cat("max_models:", max_models, "\n")
    # cat("weights_column:", weights_column, "\n")
    # cat("keep_cv:", keep_cv, "\n")
    # cat("seed:", seed, "\n")
    # print(which(!y.na))
    # print(dim(as.data.frame(bhex)))
    # print(dim(bhex[which(!y.na), ]))
    # ------------------------------------------------------------
    # fine-tune a gaussian model
    # ============================================================
    if (FAMILY[z] == 'gaussian' || FAMILY[z] == 'gaussian_integer'
        || FAMILY[z] == 'quasibinomial' ) {
      tryCatch(hex[[Y]] <- fit <- h2o::h2o.automl(x = setdiff(X, Y), y = Y,
                                                  training_frame = bhex[which(!y.na), ],
                                                  sort_metric = sort_metric,
                                                  project_name = "mlim",
                                                  include_algos = usedalgorithms,
                                                  nfolds = cv,
                                                  exploitation_ratio = 0.1,
                                                  max_runtime_secs = tuning_time,
                                                  max_models = max_models,
                                                  weights_column = weights_column[which(!y.na)],
                                                  keep_cross_validation_predictions = keep_cv,
                                                  seed = seed
                                                  # #stopping_metric = stopping_metric,
                                                  # #stopping_rounds = stopping_rounds
                                                  # #stopping_tolerance=stopping_tolerance
      ),
      error = function(cond) {
        #cat("connection to JAVA server failed...\n");
        return(NULL)})

    }

    # ------------------------------------------------------------
    # fine-tune a classification model
    # ============================================================
    else if (FAMILY[z] == 'binomial' || FAMILY[z] == 'multinomial') {

      # check balance argument (default is FALSE)
      balance_classes <- FALSE
      #sort_metric <- "mean_per_class_error" #"RMSE" #this is the default metric, not mean_per_class

      if (Y %in% balance | autobalance) {
        balance_classes <- TRUE
      }

      if (FAMILY[z] == 'binomial' & !balance_classes) {
        #sort_metric <- "AUC"
      }
      if (FAMILY[z] == 'binomial' & balance_classes) {
        #sort_metric <- "AUCPR"
      }

      #???
      #if (validation > 0) {
      #  nonMissingObs <- which(!y.na)
      #  vdFrame <- sort(sample(nonMissingObs,
      #                    round(validation * length(nonMissingObs) )))
      #  trainingsample <- sort(setdiff(which(!y.na), vdFrame))
      #}

      fit <- tryCatch(h2o::h2o.automl(x = setdiff(X, Y), y = Y,
                                      balance_classes = balance_classes,
                                      sort_metric = sort_metric,
                                      training_frame = bhex[which(!y.na), ],
                                      #validation_frame = bhex[vdFrame, ],
                                      project_name = "mlim",
                                      include_algos = usedalgorithms,
                                      nfolds = cv,
                                      exploitation_ratio = 0.1,
                                      max_runtime_secs = tuning_time,
                                      max_models = max_models,
                                      weights_column = weights_column[which(!y.na)],
                                      keep_cross_validation_predictions = keep_cv,
                                      seed = seed
                                      #stopping_metric = stopping_metric,
                                      #stopping_rounds = stopping_rounds
                                      #stopping_tolerance=stopping_tolerance
      ),
      error = function(cond) {
        #cat("connection to JAVA server failed...\n");
        return(NULL)})
    }
    else {
      stop(paste(FAMILY[z], "is not recognized"))
    }

    Sys.sleep(sleep)
    #print(fit@leaderboard)

    if (debug) {
      md.log("model fitted", trace=FALSE)
      md.log("model was executed successfully", trace=FALSE)
    }

    tryCatch(perf <- h2o::h2o.performance(fit@leader),
             error = function(cond) {
               #cat("connection to JAVA server failed...\n");
               return(NULL)})

    Sys.sleep(sleep)

    # update metrics, and if there is an improvement, update the data
    # ------------------------------------------------------------
    roundRMSE <- getDigits(tolerance) + 1
    if (roundRMSE == 1) roundRMSE <- 4

    iterationMetric <- extractMetrics(bhex, k, Y, perf, FAMILY[z])


    if (k == 1) {
      ## do not convert pred to a vector. let it be "H2OFrame"
      tryCatch(pred <- h2o::h2o.predict(fit@leader, newdata = hex[which(v.na), X])[,1],
               error = function(cond) {
                 cat("connection to JAVA server failed...\n");
                 return(NULL)})
      Sys.sleep(sleep)
      if (debug) md.log("predictions were generated", trace=FALSE)

      #h2o requires numeric subsetting
      tryCatch(hex[which(v.na), Y] <- pred,
               error = function(cond) {
                 cat("connection to JAVA server failed...\n");
                 return(NULL)})

      # RAM cleaning-ish, help needed
      tryCatch(h2o::h2o.rm(pred),
               error = function(cond) {
                 cat("connection to JAVA server failed...\n");
                 return(NULL)})

      if (debug) md.log("prediction was cleaned", trace=FALSE)
      Sys.sleep(sleep)

      # also update the bootstraped data, otherwise, update the pointer
      if (boot) {
        tryCatch(pred <- h2o::h2o.predict(fit@leader, newdata = bhex[which(y.na), X])[,1],
                 error = function(cond) {
                   cat("connection to JAVA server failed...\n");
                   return(NULL)})
        tryCatch(bhex[which(y.na), Y] <- pred,
                 error = function(cond) {
                   cat("connection to JAVA server failed...\n");
                   return(NULL)})
        Sys.sleep(sleep)
      }
      else {
        bhex <- hex
      }

      # update the metrics
      metrics <- rbind(metrics, iterationMetric)
    }
    else {
      errPrevious <- min(metrics[metrics$variable == Y, error_metric], na.rm = TRUE)
      errPrevious <- round(errPrevious, digits = 5)
      checkMetric <- iterationMetric[iterationMetric$variable == Y, error_metric]
      checkMetric <- round(checkMetric, digits = 5)

      # >>> this bit looks like I'm paranoid again... improvement needed
      # if (!is.null(errPrevious)) {
      #   if (is.na(errPrevious)) {
      #     errPrevious <- 1
      #   }
      # }
      # else errPrevious <- 1
      # <<<

      errImprovement <- checkMetric - errPrevious
      percentImprove <- (errImprovement / errPrevious)

      # IF ERROR DECREASED
      if (percentImprove < -tolerance) {
        if (debug) print(paste(round(percentImprove, 6), -tolerance))
        ## do not convert pred to a vector. let it be "H2OFrame"

        tryCatch(pred <- h2o::h2o.predict(fit@leader, newdata = hex[which(v.na), X])[,1],
                 error = function(cond) {
                   cat("connection to JAVA server failed...\n");
                   return(NULL)})
        Sys.sleep(sleep)
        if (debug) md.log("predictions were generated", trace=FALSE)
        hex[which(v.na), Y] <- pred #h2o requires numeric subsetting
        Sys.sleep(sleep)
        if (debug) md.log("data was updated in h2o cloud", trace=FALSE)

        # also update the bootstraped data, otherwise update the pointer
        if (boot) {
          tryCatch(pred <- h2o::h2o.predict(fit@leader, newdata = bhex[which(y.na), X])[,1],
                   error = function(cond) {
                     cat("connection to JAVA server failed...\n");
                     return(NULL)})
          tryCatch(bhex[which(y.na), Y] <- pred,
                   error = function(cond) {
                     cat("connection to JAVA server failed...\n");
                     return(NULL)})
          Sys.sleep(sleep)
        }
        else bhex <- hex

        # RAM cleaning-ish, help needed
        tryCatch(h2o::h2o.rm(pred),
                 error = function(cond) {
                   cat("connection to JAVA server failed...\n");
                   return(NULL)})
        if (debug) md.log("prediction was cleaned", trace=FALSE)

        # update the metrics
        metrics <- rbind(metrics, iterationMetric)
      }

      else { #if the error increased
        if (debug) print(paste("INCREASED", errImprovement, percentImprove, -tolerance))
        iterationMetric[, error_metric] <- NA
        metrics <- rbind(metrics, iterationMetric)
        if (!doublecheck) {
          ITERATIONVARS <- setdiff(ITERATIONVARS, Y)
        }
      }
    }

    tryCatch(h2o::h2o.rm(fit),
             error = function(cond) {
               #cat("connection to JAVA server failed...\n");
               return(NULL)})
    if (debug) md.log("model was cleaned", trace=FALSE)
    Sys.sleep(sleep)

    # clean h2o memory
    # ------------------------------------------------------------
    #h2o.clean(fit=fit, pred=pred, fun = "erasefit", timeout_secs = 30,
    #          FLUSH = FALSE, retained_elements = c(hexID), md.log = md.log)

    gc()
    gc()
    # tell back-end cluster nodes to do three back-to-back JVM full GCs.
    #h2o:::.h2o.garbageCollect()
    #h2o:::.h2o.garbageCollect()
    #h2o:::.h2o.garbageCollect()
  }

  # Update the predictors during the first iteration
  # ------------------------------------------------------------
  if (preimpute == "iterate" && k == 1L && (Y %in% allPredictors)) {
    X <- union(X, Y)
  }

  # .........................................................
  # POSTIMPUTATION PREPARATION
  # .........................................................
  if (!is.null(save)) {

    savestate <- list(

      # Data
      # ----------------------------------
      MI = MI,
      dataNA = dataNA,
      data=as.data.frame(hex),
      bdata=as.data.frame(bhex),
      dataLast=dataLast,
      metrics = metrics,
      mem=mem,
      orderedCols=orderedCols,

      # Loop data
      # ----------------------------------
      m = m , k = k, z=z, X=X, Y=Y, m.it = m.it,
      vars2impute=vars2impute, FAMILY=FAMILY,

      # settings
      # ----------------------------------
      ITERATIONVARS=ITERATIONVARS,
      impute=impute,
      postimpute=postimpute,
      ignore=ignore,
      autobalance = autobalance,
      balance = balance,
      save = save,
      maxiter = maxiter,
      miniter = miniter,
      cv = cv,
      tuning_time = tuning_time,
      max_models = max_models,
      matching = matching,
      ignore.rank = ignore.rank,
      weights_column = weights_column,
      seed=seed,
      verbosity=verbosity,
      verbose = verbose,
      debug = debug,
      report=report,
      flush=flush,
      error_metric=error_metric,
      tolerance=tolerance,
      error = error, #PROBABLY NOT NEEDED ???
      #stopping_metric=stopping_metric,
      #stopping_rounds=stopping_rounds,
      #stopping_tolerance=stopping_tolerance,
      cpu = cpu,
      max_ram=max_ram,
      min_ram = min_ram,
      keep_cv = keep_cv,

      # save the package version used for the imputation
      pkg=packageVersion("mlim")
    )
    class(savestate) <- "mlim"

    # update iteration data
    saveRDS(savestate, save)
  }

  # Flush the Java server to regain RAM
  # ------------------------------------------------------------
  #
  # HELP NEEDED
  #
  # this part is problematic. Contact h2o.ai for further help
  # because removing objects from the server doesn't seem to do
  # much. moreover, '.h2o.garbageCollect()' breaks some of the
  # computations for unknown reasons. for now, I can only use
  # 'gc()' and alternatively, shutdown the Java server and re-run
  # it when the RAM goes below a certain amount... this feature is
  # considered a bad practice and should be improved in future releases
  # ------------------------------------------------------------
  if (flush) {
    tryCatch(HEX <- as.data.frame(hex),
             error = function(cond) {
               #cat("connection to JAVA server failed...\n");
               return(NULL)})
    tryCatch(BHEX<- as.data.frame(bhex),
             error = function(cond) {
               #cat("connection to JAVA server failed...\n");
               return(NULL)})
    tryCatch(h2o::h2o.removeAll(timeout_secs = 30),
             error = function(cond) {
               #cat("connection to JAVA server failed...\n");
               return(NULL)})
    Sys.sleep(sleep)
    gc()
    gc()
    h2o:::.h2o.garbageCollect()
    h2o:::.h2o.garbageCollect()
    h2o:::.h2o.garbageCollect()
    Sys.sleep(sleep)
    tryCatch(hex <- h2o::as.h2o(HEX) ,
             error = function(cond) {
               #cat("connection to JAVA server failed...\n");
               return(NULL)})
    tryCatch(bhex <- h2o::as.h2o(BHEX),
             error = function(cond) {
               #cat("connection to JAVA server failed...\n");
               return(NULL)})
    #ID: data_
     #ID: data_
    Sys.sleep(sleep)
    #hexID <- h2o::h2o.getId(hex)
    if (debug) md.log("flushed", trace=FALSE)
  }

  # update the statusbar
  if (verbose==0) setTxtProgressBar(pb, z)

  return(list(X=X,
              hex = hex,
              bhex = bhex,
              metrics = metrics,
              ITERATIONVARS=ITERATIONVARS))
}
