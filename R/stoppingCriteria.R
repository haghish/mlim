#' @title iteration stopping criteria
#' @description evaluates the stopping criteria
#' @param metrics estimated error from CV
#' @param k iteration round
#' @param error_metric character. stopping metric for the iteration. default is "RMSE"
#' @return logical. if TRUE, the imputation goes on to the next iteration
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

stoppingCriteria <- function(method = "iteration_RMSE",
                             miniter, maxiter,
                             metrics, k, vars2impute,
                             error_metric,
                             tolerance,
                             postimpute, runpostimpute,
                             md.log) {

  # keep running unless...
  running <- TRUE
  error <- NA
  errImprovement <- NA

  # ............................................................
  # as long as there is a variable that has been improving,
  # keep iterating. however, if "double.check = FALSE", ignore
  # variables that do not improve in any iteration throughout
  # the rest of the iterations
  # ............................................................
  if (method == "varwise_NA") {
    # as long as there is a variable that it's RMSE is not NA, keep going!
    if (running) {
      error <- mean(metrics[metrics$iteration == k,
                            error_metric], na.rm = TRUE)

      if (is.na(error)) {

        # if all values were NA, well, stop then, if there is no postimpute!
        if (is.null(postimpute)) {
          if (is.na(error)) running <- FALSE
        }
        else {
          if (!runpostimpute) {
            runpostimpute <- TRUE
            vars2impute <- NULL #avoid the loops on the base imputer
          }
          else {
            running <- FALSE
            runpostimpute <- FALSE
          }
        }
      }



    }
  }

  # ............................................................
  # Decide the stopping criteria based on average improvement of
  # RMSE per iteration
  #
  # ............................................................
  if (method == "iteration_RMSE") {
    # there has been no (or too little) improvement, stop!
    if (running) {
      error <- mean(metrics[metrics$iteration == k,
                            error_metric], na.rm = TRUE)

      if (k == 1) cat("\n   ",error_metric,
                      " = ", round(error,4), "\n", sep = "")

      if (k >= 2) {
        # get the rmse's that made NA, because of saturation
        available <- !is.na(metrics[metrics$iteration == k, error_metric])
        errPrevious <- mean(metrics[metrics$iteration == k-1 & available,
                                    error_metric],
                            na.rm = TRUE)

        errImprovement <- error - errPrevious
        if (!is.na(error) & !is.na(errImprovement)) {
          percentImprove <- (errImprovement / errPrevious)
        }

        if (!is.na(errImprovement)) {
          if (percentImprove < 0) {
            cat("\n   ",error_metric,
                " = ", round(error,4), " (improved by ",
                round(-percentImprove*100, 3),"%)", "\n", sep = "")
          }
          else {
            cat("\n   ",error_metric,
                " = ", round(error,4), " (increased by ",
                round(percentImprove*100, 3),"%)", "\n", sep = "")
          }

          #print(paste0(error_metric,
          #            " improved by: ", round(-percentImprove*100,4),"%"))
          #running <- errImprovement < (-tolerance)
          running <- percentImprove < (-tolerance)
        }
      }
    }
  }


  # if maximum iteration has been reached and still is running...
  # ------------------------------------------------------------
  if (k == maxiter & running) {
    warning(paste("the imputation could be further improved",
                  "by increasing number of iterations.",
                  "if you have saved an mlim object via 'save.mlim'",
                  "argument, load it and specify a larger 'maxiter' to",
                  "continue imputing."))
  }

  # maximum iteration has been reached
  # ------------------------------------------------------------
  if (k == maxiter) running <- FALSE

  if (!running) runpostimpute <- FALSE

  return(list(running = running,
              error = error,
              vars2impute = vars2impute,
              improvement = errImprovement,
              runpostimpute = runpostimpute))
}
