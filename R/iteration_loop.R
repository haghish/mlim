#' @title iteration_loop
#' @description runs imputation iteration loop to fully impute a dataframe
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
iteration_loop <- function(MIit, dataNA, data, boot, metrics, tolerance, doublecheck,
                    m, k, X, Y, z,

                    # loop data
                    vars2impute, vars2postimpute, storeVars2impute,
                    allPredictors, preimpute, impute, postimpute,

                    # settings
                    error_metric, FAMILY, cv, tuning_time,
                    max_models, weights_column,
                    keep_cross_validation_predictions,
                    balance, seed, save, flush,
                    verbose, debug, report, sleep,

                    # saving settings
                    mem, orderedCols, ignore, maxiter,
                    miniter, matching, ignore.rank,
                    verbosity, error, cpu, max_ram, min_ram, shutdown, clean) {

  # bootrtap
  # ------------------------------------------------------------
  if (!boot) {
    hex <- h2o::as.h2o(data) #ID: data_
    bhex <- hex
    Sys.sleep(sleep)
    hexID <- h2o::h2o.getId(hex)
    md.log(paste("dataset ID:", hexID), trace=FALSE) #, print = TRUE
  }
  else {
    rownames(data) <- 1:nrow(data) #remember the rows that are missing
    bdata <- data[sample.int(nrow(data), nrow(data), replace=TRUE), ]
    hex <- h2o::as.h2o(data)
    bhex<- h2o::as.h2o(bdata)
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
    # update the loop
    k <- k + 1L

    # always print the iteration
    cat("\ndata ", MIit, ", iteration ", k, ":\n", sep = "") #":\t"
    md.log(paste("Iteration", k), section="section")

    if (debug) md.log("store last data", trace=FALSE)

    dataLast <- as.data.frame(hex)
    attr(dataLast, "metrics") <- metrics
    attr(dataLast, "rmse") <- error


    #>>if (debug) LASTDATA <<- dataLast

    # .........................................................
    # IMPUTATION & POSTIMPUTATION LOOP
    # .........................................................
    if (runpostimpute) procedure <- "postimpute"
    else procedure <- "impute"
    z <- 0
    for (Y in ITERATIONVARS) {

      z <- z + 1
      it <- iterate(procedure = procedure,
                    dataNA, data, bdata, boot, hex, bhex, metrics, tolerance, doublecheck,
                    m, k, X, Y, z,
                    # loop data
                    ITERATIONVARS, vars2impute,
                    allPredictors, preimpute, impute, postimpute,
                    # settings
                    error_metric, FAMILY=FAMILY, cv, tuning_time,
                    max_models, weights_column,
                    keep_cross_validation_predictions,
                    balance, seed, save, flush,
                    verbose, debug, report, sleep,
                    # saving settings
                    dataLast, mem, orderedCols, ignore, maxiter,
                    miniter, matching, ignore.rank,
                    verbosity, error, cpu, max_ram, min_ram)

      X <- it$X
      ITERATIONVARS <- it$ITERATIONVARS
      metrics <- it$metrics
      data <- it$data
      hex <- it$hex
      bhex <- it$bhex
    }

    # .........................................................
    # POSTIMPUTATION LOOP
    # .........................................................
    # if (runpostimpute) {
    #   z <- 0
    #   for (Y in vars2postimpute) {
    #     z <- z + 1
    #     it <- iterate(procedure = "postimpute",
    #                   dataNA, data, bdata, boot, hex, bhex=NULL, metrics, tolerance, doublecheck,
    #                   k, X, Y, z,
    #                   # loop data BUT ADD POSTIMPUTE TWICE??? fix it by seperating saving
    #                   ITERATIONVARS, vars2impute,
    #                   allPredictors, preimpute, impute, postimpute,
    #                   # settings
    #                   error_metric, FAMILY=FAMILY, cv, tuning_time,
    #                   max_models, weights_column,
    #                   keep_cross_validation_predictions,
    #                   balance, seed, save, flush,
    #                   verbose, debug, report, sleep,
    #                   # saving settings
    #                   dataLast, mem, orderedCols, ignore, maxiter,
    #                   miniter, matching, ignore.rank,
    #                   verbosity, error, cpu, max_ram, min_ram)
    #
    #     X <- it$X
    #     vars2postimpute <- it$vars2impute
    #     metrics <- it$metrics
    #     hex <- it$hex
    #   }
    # }

    # CHECK CRITERIA FOR RUNNING THE NEXT ITERATION
    # --------------------------------------------------------------
    if (debug) md.log("evaluating stopping criteria", trace=FALSE)
    SC <- stoppingCriteria(method="varwise_NA", miniter, maxiter,
                           metrics, k, vars2impute,
                           error_metric,
                           tolerance,
                           postimpute,
                           runpostimpute,
                           md.log = report)
    if (debug) {
      md.log(paste("running: ", SC$running), trace=FALSE)
      md.log(paste("Estimated",error_metric,
                   "error:", SC$error), trace=FALSE)
    }

    running <- SC$running
    error <- SC$error
    runpostimpute <- SC$runpostimpute
    ITERATIONVARS <- SC$vars2impute #only sets it to NULL
  }

  # reset vars2impute because it was altered to exit the loops and
  # setup postimputation
  # --------------------------------------------------------------
  #vars2impute <- storeVars2impute

  # ............................................................
  # END OF THE ITERATIONS
  # ............................................................
  if (verbose) cat("\n\n")

  md.log("This is the end, beautiful friend...", trace=FALSE)

  # if the iterations stops on minimum or maximum, return the last data
  if (k == miniter || (k == maxiter && running) || maxiter == 1) {
    md.log("limit reached", trace=FALSE)
    dataLast <- as.data.frame(hex)
    Sys.sleep(sleep)
    attr(dataLast, "metrics") <- metrics
    attr(dataLast, error_metric) <- error
  }
  else {
    md.log("return previous iteration's data", trace=FALSE)
  }

  if (clean) h2o::h2o.removeAll()

  if (shutdown) {
    md.log("shutting down the server", trace=FALSE)
    h2o::h2o.shutdown(prompt = FALSE)
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
        #print(matchedVal)
        if (!is.null(matchedVal)) dataLast[v.na, Y] <- matchedVal
        else {
          md.log("matching failed", trace=FALSE)
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
