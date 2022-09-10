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
                    allPredictors, preimpute, impute, postimputealgos,

                    # settings
                    error_metric, FAMILY, cv, tuning_time,
                    max_models,
                    keep_cv,
                    autobalance, balance, seed, save, flush,

                    verbose, debug, report, sleep,

                    # saving settings
                    mem, orderedCols, ignore, maxiter,
                    miniter, matching, ignore.rank,
                    verbosity, error, cpu, max_ram, min_ram
                    ) {

  # for the first dataframe imputation
  #if (is.null(bhex)) bhex <- hex

  #ZZ <- z
  #message(paste("z:",z, "length:",  length(vars2impute)))
  #if (z >= length(vars2impute)) ZZ <- length(vars2impute) - 1

  if (verbose==0) pb <- txtProgressBar(z-1, length(vars2impute), style = 3)
  if (debug) message(paste0(Y," (RAM = ", memuse::Sys.meminfo()$freeram,")\n"))

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
    if (debug) md.log("uni impute", date=debug, time=debug, trace=FALSE)
    #??? change this with a self-written function
    tryCatch(hex[[Y]] <- h2o::as.h2o(missRanger::imputeUnivariate(data[[Y]])),
             error = function(cond) {
               #message("connection to JAVA server failed...\n");
               return(stop("preimputation failed! \nuse 'mlim.preimpute' function and pass it via 'preimputed.data' to mlim"))})
    Sys.sleep(sleep)
  }
  else {

    if (debug) md.log(paste("X:",paste(setdiff(X, Y), collapse = ", ")), date=debug, time=debug, trace=FALSE)
    if (debug) md.log(paste("Y:",paste(Y, collapse = ", ")), date=debug, time=debug, trace=FALSE)

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
    else usedalgorithms <- postimputealgos
    # if (debug) message(paste("usedalgorithms", usedalgorithms))
    # message("X:", X, "\n")
    # message("Y:", Y, "\n")
    # message("sort_metric:", sort_metric, "\n")
    # message("usedalgorithms:", usedalgorithms, "\n")
    # message("cv:", cv, "\n")
    # message("tuning_time:", tuning_time, "\n")
    # message("max_models:", max_models, "\n")
    # message("keep_cv:", keep_cv, "\n")
    # message("seed:", seed, "\n")

# dodo <<- as.data.frame(hex[which(!y.na), ])
# bobo <<- as.data.frame(hex)
# print(h2o.dim(hex))
# print(h2o.getId(hex))

    # ------------------------------------------------------------
    # fine-tune a gaussian model
    # ============================================================
    if (FAMILY[z] == 'gaussian' || FAMILY[z] == 'gaussian_integer'
        || FAMILY[z] == 'quasibinomial' ) {
      tryCatch(fit <- h2o::h2o.automl(x = setdiff(X, Y), y = Y,
                                      training_frame = if (is.null(bhex)) hex[which(!y.na), ] else bhex[which(!y.na), ],
                                      sort_metric = sort_metric,
                                      project_name = "mlim",
                                      include_algos = usedalgorithms,
                                      nfolds = cv,
                                      exploitation_ratio = 0.1,
                                      max_runtime_secs = tuning_time,
                                      max_models = max_models,
                                      weights_column = if (is.null(bhex)) NULL else "mlim_bootstrap_weights_column_", #adjusted_weight_column[which(!y.na)],
                                      keep_cross_validation_predictions = keep_cv,
                                      seed = seed
                                      # #stopping_metric = stopping_metric,
                                      # #stopping_rounds = stopping_rounds
                                      # #stopping_tolerance=stopping_tolerance
      ),
      error = function(cond) {
        message(paste("Model training for variable", Y, "failed... see the Java server error below:\n"));
        return(stop(cond))})

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

      ## SHOULD MLIM INCLUDE DIFFERENT MODEL EVALUATION METRICS BASED ON VARTYPE?
      ## FOR NOW, KEEP IT TO THE DEFAULT
      # if (FAMILY[z] == 'binomial' & !balance_classes) {
      #   #sort_metric <- "AUC"
      # }
      # if (FAMILY[z] == 'binomial' & balance_classes) {
      #   #sort_metric <- "AUCPR"
      # }

      #???
      #if (validation > 0) {
      #  nonMissingObs <- which(!y.na)
      #  vdFrame <- sort(sample(nonMissingObs,
      #                    round(validation * length(nonMissingObs) )))
      #  trainingsample <- sort(setdiff(which(!y.na), vdFrame))
      #}

      tryCatch(fit <- h2o::h2o.automl(x = setdiff(X, Y), y = Y,
                                      balance_classes = balance_classes,
                                      sort_metric = sort_metric,
                                      training_frame = if (is.null(bhex)) hex[which(!y.na), ] else bhex[which(!y.na), ],
                                      #validation_frame = bhex[vdFrame, ],
                                      project_name = "mlim",
                                      include_algos = usedalgorithms,
                                      nfolds = cv,
                                      exploitation_ratio = 0.1,
                                      max_runtime_secs = tuning_time,
                                      max_models = max_models,
                                      weights_column = if (!balance_classes) {if (is.null(bhex)) NULL else "mlim_bootstrap_weights_column_"} else NULL, #adjusted_weight_column[which(!y.na)],
                                      keep_cross_validation_predictions = keep_cv,
                                      seed = seed
                                      #stopping_metric = stopping_metric,
                                      #stopping_rounds = stopping_rounds
                                      #stopping_tolerance=stopping_tolerance
      ),
      error = function(cond) {
        message("model training failed. perhaps low RAM problem?...\n");
        return(stop(cond))})
    }
    else {
      stop(paste(FAMILY[z], "is not recognized"))
    }

# fit <<- fit
# print(h2o.dim(hex))
# print(h2o.getId(hex))

    Sys.sleep(sleep)
    #message(fit@leaderboard)
    if (debug) md.log("model fitted", date=debug, time=debug, trace=FALSE)
    if (debug) md.log(paste("leader:", fit@leader@model_id), date=debug, time=debug, trace=FALSE)



    tryCatch(perf <- h2o::h2o.performance(fit@leader, xval = TRUE),
             error = function(cond) {
               message("Model performance evaluation failed...\n");
               message("Java server crashed. perhaps a RAM problem?\n")
               return(stop(cond))})

    Sys.sleep(sleep)

    # update metrics, and if there is an improvement, update the data
    # ------------------------------------------------------------
    roundRMSE <- getDigits(tolerance) + 1
    if (roundRMSE == 1) roundRMSE <- 4

    iterationMetric <- extractMetrics(if (is.null(bhex)) hex else bhex, k, Y, perf, FAMILY[z])


    if (k == 1) {
      ## do not convert pred to a vector. let it be "H2OFrame"
      tryCatch(pred <- h2o::h2o.predict(fit@leader, newdata = hex[which(v.na), X])[,1],
               error = function(cond) {
                 message("generating the missing data predictions failed... see the Java server error below:\n");
                 return(stop(cond))})
      Sys.sleep(sleep)
      if (debug) md.log("predictions were generated", date=debug, time=debug, trace=FALSE)

      #h2o requires numeric subsetting
      tryCatch(hex[which(v.na), Y] <- pred,
               error = function(cond) {
                 message("connection to JAVA server failed...\n");
                 return(stop("Java server crashed. perhaps a RAM problem?"))})

      # RAM cleaning-ish, help needed
      tryCatch(h2o::h2o.rm(pred),
               error = function(cond) {
                 message("connection to JAVA server failed...\n");
                 return(stop("Java server crashed. perhaps a RAM problem?"))})

      if (debug) md.log("prediction was cleaned", date=debug, time=debug, trace=FALSE)
      Sys.sleep(sleep)

      # also update the bootstraped data, otherwise, update the pointer
      if (boot) {
        tryCatch(pred <- h2o::h2o.predict(fit@leader, newdata = bhex[which(y.na), X])[,1],
                 error = function(cond) {
                   message("connection to JAVA server failed...\n");
                   return(stop("Java server crashed. perhaps a RAM problem?"))})
        tryCatch(bhex[which(y.na), Y] <- pred,
                 error = function(cond) {
                   message("connection to JAVA server failed...\n");
                   return(stop("Java server crashed. perhaps a RAM problem?"))})
        Sys.sleep(sleep)
      }
      # else if (!is.null(bhex)) bhex <- hex


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
        if (debug) message(paste(round(percentImprove, 6), -tolerance))
        ## do not convert pred to a vector. let it be "H2OFrame"

        tryCatch(pred <- h2o::h2o.predict(fit@leader, newdata = hex[which(v.na), X])[,1],
                 error = function(cond) {
                   message("connection to JAVA server failed...\n");
                   return(stop("Java server crashed. perhaps a RAM problem?"))})
        Sys.sleep(sleep)
        if (debug) md.log("predictions were generated", date=debug, time=debug, trace=FALSE)
        hex[which(v.na), Y] <- pred #h2o requires numeric subsetting
        Sys.sleep(sleep)
        if (debug) md.log("data was updated in h2o cloud", date=debug, time=debug, trace=FALSE)

        # also update the bootstraped data, otherwise update the pointer
        if (boot) {
          tryCatch(pred <- h2o::h2o.predict(fit@leader, newdata = bhex[which(y.na), X])[,1],
                   error = function(cond) {
                     message("connection to JAVA server failed...\n");
                     return(stop("Java server crashed. perhaps a RAM problem?"))})
          tryCatch(bhex[which(y.na), Y] <- pred,
                   error = function(cond) {
                     message("connection to JAVA server failed...\n");
                     return(stop("Java server crashed. perhaps a RAM problem?"))})
          Sys.sleep(sleep)
        }
        # else if (!is.null(bhex)) bhex <- hex

        # RAM cleaning-ish, help needed
        tryCatch(h2o::h2o.rm(pred),
                 error = function(cond) {
                   message("connection to JAVA server failed...\n");
                   return(stop("Java server crashed. perhaps a RAM problem?"))})
        if (debug) md.log("prediction was cleaned", date=debug, time=debug, trace=FALSE)

        # update the metrics
        metrics <- rbind(metrics, iterationMetric)
      }

      else { #if the error increased
        if (debug) message(paste("INCREASED", errImprovement, percentImprove, -tolerance))
        iterationMetric[, error_metric] <- NA
        metrics <- rbind(metrics, iterationMetric)
        if (!doublecheck) {
          ITERATIONVARS <- setdiff(ITERATIONVARS, Y)
        }
      }
    }

    tryCatch(h2o::h2o.rm(fit),
             error = function(cond) {
               #message("connection to JAVA server failed...\n");
               return(stop("Java server crashed. perhaps a RAM problem?"))})
    if (debug) md.log("model was cleaned", date=debug, time=debug, trace=FALSE)
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

  if (debug) md.log("itteration done", date=debug, time=debug, trace=FALSE)

  # Update the predictors during the first iteration
  # ------------------------------------------------------------
  if (preimpute == "iterate" && k == 1L && (Y %in% allPredictors)) {
    X <- union(X, Y)
    if (debug) md.log("x was updated", date=debug, time=debug, trace=FALSE)
  }

  # .........................................................
  # POSTIMPUTATION PREPARATION
  # .........................................................
  if (!is.null(save)) {
    if (debug) md.log("Saving the status", date=debug, time=debug, trace=FALSE)
    savestate <- list(

      # Data
      # ----------------------------------
      MI = MI,
      dataNA = dataNA,
      #data=as.data.frame(hex), #??? update this to only download the imputed vector
      #bdata=as.data.frame(bhex),
      hexID = h2o.getId(hex),
      hexPATH = paste0(getwd(), "/.flush"),
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
      postimputealgos=postimputealgos,
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

  if (debug) md.log("saving done!", date=debug, time=debug, trace=FALSE)

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

  ####### FLUSHING (THE SHITY RAM MANAGEMENT OF JAVA!)
  ####### ===================================================================
  ### The reason for flushing is that there are some RAM management issues
  ### with the h2o server and even after removing the frames, Java does not
  ### flush well. Removing objects, while retaining the needed frames gave
  ### some WEIRD bugs as well, so I gave up in the process. It seemed that
  ### the data had more than one ID (there is another associated frame...).
  ### weirdly, if I only remove the generated model, the RAM just keeps
  ### accumulating, causing the server to crash. Well, this needs to be
  ### validated in the future. in the process of development, I had to come
  ### up with quick solutions to complete the first version of the algorithm...
  ### on top of all this, .h2o.garbageCollect() function is not exported
  ### and CRAN does not accepts it either. When I was trying the h2o.removeAll()
  ### function, sometimes even listing the objects of Java was returning an error.
  ### My understanding was that the Java server is highly RAM dependent and
  ### as the RAM becomes occupied, the server becomes unstable...
  ###
  ### In the SOLUTION1, the data is downloaded to R, the whole server is
  ### flushed, and then the data is reuploaded. I know, that is more of a
  ### disease than a medicine! For a larger datasets, it becomes EXTREMELY
  ### slow to download data from Java server to R. This itself became a
  ### bug because sometimes the server would fail downloading a very large
  ### dataset!
  ###
  ### In SOLUTION 2, the data is downloaded to the disk in native h2o format,
  ### the server is entirely flushed, and then the data is uploaded again.
  ### THIS SOLUTION IS SO MUCH FASTER and is currently implemented, until
  ### I realize a proper solution what the hell should I do with the RAM
  ### management. CURRENTLY, SOLUTION 2 IS IMPLEMENTED.
  ###
  ### SOLUTION 3: A perhaps a better solution would be to update the original
  ### data in R, save it as RDS, and then flush the server and reupload.
  ### storing the dataframe in h2o format was chunky and it will create a lot
  ### of files, although the reading speed was quite high. MOREOVER, the
  ### as.data.frame() function should be avoided anywhere in the package.
  ### it is so slow and unstable, can easily crash with large datasets.
  ###
  ### NOTE: If you'd like to use h2o.removeAll(), beware that retaining the
  ### dataframe ID will cause a bug. if you prefer using h2o.rm(), make sure
  ### it is effective!

  ### The primary solution was to download the frame from Java to R,
  ### clean Java, and re-upload the frames to Java.

  if (flush) {
    if (debug) md.log("flushing the server...", date=debug, time=debug, trace=FALSE)

    ####### SOLUTION 2: save on disk > flush Java > reupload
    ####### =================================================

    # 0. ERASE the .flush directory
    tryCatch(do.call(file.remove,
                     list(list.files(paste0(getwd(), "/.flush"),
                                     full.names = TRUE))),
             error = function(cond) {
               message("mlim could not flush the temporary data...\n Perhaps restricted access permission?\n");
               return()})

    # 1. SAVE
    hexID <- h2o.getId(hex)
    h2o.save_frame(hex, dir = paste0(getwd(), "/.flush"))
    if (!is.null(bhex)) {
      bhexID <- h2o.getId(bhex)
      h2o.save_frame(bhex, dir = paste0(getwd(), "/.flush"))
    }
    else bhexID <- NULL
    mlimID <- list(hexID, bhexID)
    class(mlimID) <- "mlim.id"
    saveRDS(object = list(hexID, bhexID), file = paste0(getwd(), "/.flush/mlim.id"))
    if (debug) md.log("data stored", date=debug, time=debug, trace=FALSE)

    # 2. FLUSH
    tryCatch(h2o::h2o.removeAll(),
             error = function(cond) {
               #message("connection to JAVA server failed...\n");
               return(stop("Java server crashed. perhaps a RAM problem?"))})
    Sys.sleep(sleep)
    gc()
    gc()
    if (debug) md.log("server flushed", date=debug, time=debug, trace=FALSE)

    # 3. REUPLOAD
    tryCatch(hex <- h2o::h2o.load_frame(hexID, dir = paste0(getwd(), "/.flush")) ,
             error = function(cond) {
               #message("connection to JAVA server failed...\n");
               return(stop("Java server crashed. perhaps a RAM problem?"))})
    hex <- as.h2o(hex)
    if (!is.null(bhexID)) {
      tryCatch(bhex <- h2o::h2o.load_frame(bhexID, dir = paste0(getwd(), "/.flush")),
               error = function(cond) {
                 #message("connection to JAVA server failed...\n");
                 return(stop("Java server crashed. perhaps a RAM problem?"))})
      bhex <- as.h2o(bhex)
    }
    #ID: data_
    #ID: data_
    if (debug) md.log("data reuploaded", date=debug, time=debug, trace=FALSE)
# md.log(h2o.getId(hex))
# md.log(h2o.dim(hex))
# aa <<- as.data.frame(hex)


    Sys.sleep(sleep)

    ####### SOLUTION 1: store in RAM > flush Java > reupload
    ####### =================================================
    # catch <- NULL
    # catchN<- 0
    # while(is.null(NULL)) {
    #   catch <- tryCatch(HEX <- as.data.frame(hex),
    #            error = function(cond) {
    #              #message("connection to JAVA server failed...\n");
    #              return(NULL)})
    #   if (is.null(catch)) {
    #     catchN <- catchN + 1
    #     message(paste("Try",catchN,":",
    #                   "data.frame could not be downloaded from Java server. will try again in 10 seconds...\n"))
    #     Sys.sleep(10)
    #   }
    #
    #   if (catchN > 3) stop("data.frame could not be downloaded from Java server.")
    # }
    #
    # if (!is.null(bhex)) tryCatch(BHEX<- as.data.frame(bhex),
    #          error = function(cond) {
    #            #message("connection to JAVA server failed...\n");
    #            return(stop("Java server crashed. perhaps a RAM problem?"))})
    #
    #
    #
    # tryCatch(h2o::h2o.removeAll(),
    #          error = function(cond) {
    #            #message("connection to JAVA server failed...\n");
    #            return(stop("Java server crashed. perhaps a RAM problem?"))})
    # Sys.sleep(sleep)
    # gc()
    # gc()
    #
    # ### NOT ALLOWED ON CRAN :/
    # #h2o:::.h2o.garbageCollect()
    # #h2o:::.h2o.garbageCollect()
    # #h2o:::.h2o.garbageCollect()
    # #Sys.sleep(sleep)
    # tryCatch(hex <- h2o::as.h2o(HEX) ,
    #          error = function(cond) {
    #            #message("connection to JAVA server failed...\n");
    #            return(stop("Java server crashed. perhaps a RAM problem?"))})
    # if (!is.null(bhex)) tryCatch(bhex <- h2o::as.h2o(BHEX),
    #          error = function(cond) {
    #            #message("connection to JAVA server failed...\n");
    #            return(stop("Java server crashed. perhaps a RAM problem?"))})
    # #ID: data_
    #  #ID: data_
    # Sys.sleep(sleep)
    # #hexID <- h2o::h2o.getId(hex)
    #
    # if (debug) md.log("flushing completed", trace=FALSE)
  }

  # IF NOT FLUSHING, BUT SAVING, STILL SAVE THE DATA TO FLUSH
  # ---------------------------------------------------------
  # if (!flush & !is.null(save)) {
  #   tryCatch(do.call(file.remove,
  #                    list(list.files(paste0(getwd(), "/.flush"),
  #                                    full.names = TRUE))),
  #            error = function(cond) {
  #              message("mlim could not flush the temporary data...\n Perhaps restricted access permission?\n");
  #              return()})
  #   hexID <- h2o.getId(hex)
  #   mlimID <- list(hexID, bhexID = NULL)
  #   class(mlimID) <- "mlim.id"
  #   saveRDS(object = list(hexID, bhexID), file = "mlim.id")
  #   h2o.save_frame(hex, dir = paste0(getwd(), "/.flush"))
  # }

  if (debug) md.log("flushing done!", date=debug, time=debug, trace=FALSE)

  # update the statusbar
  if (verbose==0) setTxtProgressBar(pb, z)

  if (debug) md.log("return to loop!", date=debug, time=debug, trace=FALSE)

# print(h2o.dim(hex))
# print(h2o.getId(hex))
  return(list(X=X,
              hex = hex,
              bhex = bhex,
              metrics = metrics,
              ITERATIONVARS=ITERATIONVARS))
}
