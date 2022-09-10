#' @title iteration_loop
#' @description runs imputation iteration loop to fully impute a dataframe
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

iteration_loop <- function(MI, dataNA, data, bdata, boot, metrics, tolerance, doublecheck,
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
                    verbosity, error, cpu, max_ram, min_ram, shutdown, clean) {

  # bootrtap
  # ------------------------------------------------------------
  if (!boot) {
     #ID: data_
    tryCatch(hex <- h2o::as.h2o(data),
             error = function(cond) {
               message("trying to upload data to JAVA server...\n");
               message("ERROR: Data could not be uploaded to the Java Server\nJava server returned the following error:\n")
               return(stop(cond))})
    bhex <- NULL
    Sys.sleep(sleep)
  }

  #### PROBLEM
  #### drop the duplicates because they screw up the k-fold cross-validation.
  #### multiple identical observations might go to train and test datasets.

  else {
    rownames(data) <- 1:nrow(data) #remember the rows that are missing
    #??? it doesn't matter. you can always bootstrap at the beginning
    #if (is.null(bdata)) {
      sampling_index <- sample.int(nrow(data), nrow(data), replace=TRUE)

      ## SOLUTION 1: DROP THE DUPLICATES AND DO UNDERSAMPLING
      ## ----------------------------------------------------
      # sampling_index <- sampling_index[!duplicated(sampling_index)]
      # bdata <- data[sampling_index, ]

      ## SOLUTION 2: ADD THE DUPLICATES TO THE WEIGHT_COLUMN
      ## ----------------------------------------------------
      dups <- bootstrapWeight(sampling_index)
      bdata <- data[1:nrow(data) %in% dups[,1], ]

      # calculate bootstrap weight
      bdata[, "mlim_bootstrap_weights_column_"] <- dups[,2]

      ####### ===============================================
      ####### BOOTSTRAP AND BALANCING DRAMA
      ####### ===============================================
      #??? THIS NEEDS FURTHER UPDATE IF 'autobalance' IS ACTIVATED
      # THE SOlUTION WOULD BE TO CALCULATE BALANCING WEIGHTS FOR
      # EACH OBSERVATION AND THEN MULTIPLY IT BY THE WEIGHTS_COLUMN.
      # OR CARRY OUT BALANCED STRATIFIED SAMPLING FOR CATEGORICAL
      # VARIABLES...

    #}

    tryCatch(hex <- h2o::as.h2o(data),
             error = function(cond) {
               message("trying to upload data to JAVA server...\n");
               message("ERROR: Data could not be uploaded to the Java Server\nJava server returned the following error:\n")
               return(stop(cond))})

    tryCatch(bhex<- h2o::as.h2o(bdata),
             error = function(cond) {
               message("trying to upload data to JAVA server...\n");
               message("ERROR: Data could not be uploaded to the Java Server\nJava server returned the following error:\n")
               return(stop(cond))})
  }
  Sys.sleep(sleep)

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

  while (running) {

    # always print the iteration
    message(paste0("\ndata ", m.it, ", iteration ", k, " (RAM = ", memuse::Sys.meminfo()$freeram,")", ":"), sep = "") #":\t"
    md.log(paste("Iteration", k), section="section")

    # ## AVOID THIS PRACTICE BECAUSE DOWNLOADING DATA FROM THE SERVER IS SLOW
    # # store the last data
    # if (debug) md.log("store last data", date=debug, time=debug, trace=FALSE)
    # dataLast <- as.data.frame(hex)
    # attr(dataLast, "metrics") <- metrics
    # attr(dataLast, "rmse") <- error

    # .........................................................
    # IMPUTATION & POSTIMPUTATION LOOP
    # .........................................................
    if (runpostimpute) procedure <- "postimpute"
    else procedure <- "impute"

    for (Y in ITERATIONVARS[z:length(ITERATIONVARS)]) {
      start = as.integer(Sys.time())
# print(h2o.getId(hex))
# print(h2o.dim(hex))
      it <- iterate(procedure = procedure,
                    MI, dataNA, data, bdata, boot, hex, bhex, metrics, tolerance, doublecheck,
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
                    verbosity, error, cpu, max_ram, min_ram)

      X             <- it$X
      ITERATIONVARS <- it$iterationvars
      metrics       <- it$metrics
      data          <- it$data
      bdata         <- it$bdata
      hex           <- it$hex
      bhex          <- it$bhex

      time = as.integer(Sys.time()) - start
      if (debug) md.log(paste("done! after: ", time, " seconds"), time = TRUE, print = TRUE)
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
      md.log(paste("Estimated",error_metric, "error:", SC$error), date=debug, time=debug, trace=FALSE)
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

    # update the loop
    k <- k + 1L
  }

  # ............................................................
  # END OF THE ITERATIONS
  # ............................................................
  if (verbose) message("\n\n")

  md.log("This is the end, beautiful friend...", date=debug, time=debug, trace=FALSE)

  # # if the iterations stops on minimum or maximum, return the last data
  # if (k == miniter || (k == maxiter && running) || maxiter == 1) {
  ###### ALWAYS RETURN THE LAST DATA. THIS WAS A BUG, REMAINING AFTER I INDIVIDUALIZED IMPUTATION EVALUATION
  md.log("limit reached", date=debug, time=debug, trace=FALSE)
  dataLast <- as.data.frame(hex)
  Sys.sleep(sleep)
  attr(dataLast, "metrics") <- metrics
  attr(dataLast, error_metric) <- error
  # }
  # else {
  #   md.log("return previous iteration's data", date=debug, time=debug, trace=FALSE)
  # }


  if (clean) {
    tryCatch(h2o::h2o.removeAll(),
             error = function(cond) {
               message("trying to connect to JAVA server...\n");
               return(stop("Java server has crashed (low RAM?)"))})
    md.log("server was cleaned", date=debug, time=debug, trace=FALSE)
  }

  if (shutdown) {
    md.log("shutting down the server", date=debug, time=debug, trace=FALSE)
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
    z <- 0
    for (Y in vars2impute) {
      z <- z + 1
      v.na <- dataNA[, Y]

      if ((FAMILY[z] == 'gaussian_integer') | (FAMILY[z] == 'quasibinomial')) {
        if (debug) md.log(paste("matching", Y))

        matchedVal <- matching(imputed=dataLast[v.na, Y],
                               nonMiss=unique(dataLast[!v.na,Y]),
                               md.log)
        #message(matchedVal)
        if (!is.null(matchedVal)) dataLast[v.na, Y] <- matchedVal
        else {
          md.log("matching failed", date=debug, time=debug, trace=FALSE)
        }
      }
    }
  }

  # ------------------------------------------------------------
  # Revert ordinal transformation
  # ============================================================
  if (!ignore.rank) {
    dataLast[, orderedCols] <-  revert(dataLast[, orderedCols, drop = FALSE], mem)
  }

  class(dataLast) <- c("mlim", "data.frame")

  return(dataLast=dataLast)
}
