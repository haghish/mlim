#' @title iteration_loop
#' @description runs imputation iteration loop to fully impute a dataframe
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom h2o h2o.init as.h2o h2o.predict h2o.ls
#'             h2o.removeAll h2o.rm h2o.shutdown h2o.get_automl
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit rnorm
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

iteration_loop <- function(MI, dataNA, preimputed.data, data, bdata, boot, metrics, tolerance, doublecheck,
                    m, k, X, Y, z, m.it,

                    # loop data
                    vars2impute, vars2postimpute, storeVars2impute,
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
                    verbosity, error, cpu, max_ram, min_ram, shutdown, clean,
                    stochastic) {

  FACTORPREDCTIONS <- NULL

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

  ####### ===============================================
  ####### BOOTSTRAP AND BALANCING DRAMA
  ####### ===============================================
  #??? THIS NEEDS FURTHER UPDATE IF 'autobalance' IS ACTIVATED
  # THE SOlUTION WOULD BE TO CALCULATE BALANCING WEIGHTS FOR
  # EACH OBSERVATION AND THEN MULTIPLY IT BY THE WEIGHTS_COLUMN.
  # OR CARRY OUT BALANCED STRATIFIED SAMPLING FOR CATEGORICAL
  # VARIABLES...

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
  running       <- TRUE
  runpostimpute <- FALSE

  if (debug) md.log("data was sent to h2o cloud", date=debug, time=debug, trace=FALSE)

  # define iteration var. this is a vector of varnames that should be imputed
  # if 'doublecheck' argument is FALSE, everytime a variable stops improving,
  # remove it from ITERATIONVARS. When you get to postimputation, reset the
  # ITERATIONVARS.
  ITERATIONVARS <- vars2impute

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

    # always print the iteration
    message(paste0("\ndata ", m.it, ", iteration ", k, " (RAM = ", memuse::Sys.meminfo()$freeram,")", ":"), sep = "") #":\t"
    md.log(paste("Iteration", k), section="subsection")

    # ## AVOID THIS PRACTICE BECAUSE DOWNLOADING DATA FROM THE SERVER IS SLOW
    # # store the last data
    # if (debug) md.log("store last data", date=debug, time=debug, trace=FALSE)
    # dataLast <- as.data.frame(hex)
    # attr(dataLast, "metrics") <- metrics
    # attr(dataLast, "rmse") <- error

    # .........................................................
    # IMPUTATION & POSTIMPUTATION LOOP
    # .........................................................
    if (runpostimpute) {
      procedure <- "postimpute"
      if (debug) md.log("Running POSTIMPUTATION", date = TRUE, time = TRUE, print = FALSE, trace = FALSE)
    }
    else procedure <- "impute"

    for (Y in ITERATIONVARS[z:length(ITERATIONVARS)]) {
      start <- as.integer(Sys.time())

      # Prepare the progress bar and iteration console text
      # ============================================================
      if (verbose==0) pb <- txtProgressBar((which(ITERATIONVARS == Y))-1, length(vars2impute), style = 3)
      if (verbose!=0) message(paste0("    ",Y))

      it <- NULL
      tryCatch(capture.output(
          it <- iterate(
            procedure = procedure,
            MI, dataNA, bdataNA,
            preimputed.data, data, bdata, boot, hex, bhex, metrics, tolerance, doublecheck,
            m, k, X, Y, z=which(ITERATIONVARS == Y), m.it,
            # loop data
            ITERATIONVARS, vars2impute,
            allPredictors, preimpute, impute, postimputealgos,
            # settings
            error_metric, FAMILY=FAMILY, cv, tuning_time,
            max_models,
            keep_cv,
            autobalance, balance, seed, save, flush,
            verbose, debug, report, sleep,
            # saving settings
            mem, orderedCols, ignore, maxiter,
            miniter, matching, ignore.rank,
            verbosity, error, cpu, max_ram, min_ram, stochastic)
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

        # if 'factorPred' is not NULL, update the list:
        if (!is.null(it$factorPred)) {
          #remove the 'predict' column, which is the first column in predict dataframe
          #Ok <<- it$factorPred[,2:ncol(it$factorPred)]
          if (length(FACTORPREDCTIONS) > 0) FACTORPREDCTIONS <- list(FACTORPREDCTIONS, Y = it$factorPred[,2:ncol(it$factorPred)])
          else FACTORPREDCTIONS <- list(Y = it$factorPred[,2:ncol(it$factorPred)])

          # update the name of the new item
          names(FACTORPREDCTIONS)[length(FACTORPREDCTIONS)] <- Y
        }
      }

      else tryCatch(h2o::h2o.rm(h2o::h2o.get_automl("mlim")),
                    error = function(cond) {
                      return(NULL)})

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
                           postimputealgos,
                           runpostimpute,
                           md.log = report)
    if (debug) {
      md.log(paste("running: ", SC$running), date=debug, time=debug, trace=FALSE)
      md.log(paste("\nEstimated", error_metric, "error:", SC$error), section="paragraph", date=debug, time=debug, trace=FALSE)
    }

    running <- SC$running
    error <- SC$error
    runpostimpute <- SC$runpostimpute
    ITERATIONVARS <- SC$vars2impute #only sets it to NULL

    # indication of postimpute
    if (length(ITERATIONVARS) == 0) {
      ITERATIONVARS <- vars2impute
      procedure <- "postimpute"
    }

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
  attr(data, "metrics") <- metrics
  attr(data, error_metric) <- error
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
  # Adding stochastic variation
  #
  # Note: for continuous variables, RMSE is used as indication of
  #       standard deviation. However, for binomial and multinomial
  #       variables, the estimated probability of each level for
  #       each missing observation is needed and thus, the predictions
  #       of these variables should be stored and used in this section.
  # ============================================================
  if (stochastic) {
    md.log("STOCHASTIC TIMES", section="paragraph", trace=FALSE)

    # evaluate each variable and add stochastic variation based on variable types
    # ---------------------------------------------------------------------------
    for (Y in vars2impute) {
      v.na <- dataNA[, Y]
      VEK <- data[which(v.na), Y]

      if (FAMILY[which(ITERATIONVARS == Y)] == 'gaussian' ||
          FAMILY[which(ITERATIONVARS == Y)] == 'gaussian_integer'
          || FAMILY[which(ITERATIONVARS == Y)] == 'quasibinomial' ) {

        RMSE <- min(metrics[metrics$variable == Y, "RMSE"], na.rm = TRUE)
        data[which(v.na), Y] <- rnorm(
          n = length(VEK),
          mean = VEK,
          sd = RMSE)
      }

      else if (FAMILY[which(ITERATIONVARS == Y)] == 'binomial' ||
               FAMILY[which(ITERATIONVARS == Y)] == 'multinomial') {

#NOK <<- FACTORPREDCTIONS
        #> #column names represent the estimated levels' probabilities
        stochFactors <- stochasticFactorImpute(levels = colnames(FACTORPREDCTIONS[[Y]]),
                                               probMat = as.matrix(FACTORPREDCTIONS[[Y]]))

        # replace the missing observations with the stochastic data
        data[which(v.na), Y] <- stochFactors
      }

    }
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

  class(data) <- c("mlim", "data.frame")

  return(dataLast=data)
}
