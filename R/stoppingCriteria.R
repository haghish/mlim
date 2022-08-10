#' @title iteration stopping criteria
#' @description evaluates the stopping criteria
#' @param metrics estimated error from CV
#' @param k iteration round
#' @param iteration_stopping_metric character. stopping metric for the iteration
#' @return logical. if TRUE, the imputation goes on to the next iteration
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

stoppingCriteria <- function(miniter, maxiter,
                             metrics, k, vars2impute,
                             iteration_stopping_metric,
                             iteration_stopping_tolerance,
                             md.log) {

  # keep running unless...
  running <- TRUE
  error <- NA
  errImprovement <- NA



  # there has been no (or too little) improvement
  # ------------------------------------------------------------
  if (running) {
    error <- mean(metrics[metrics$iteration == k,
                          iteration_stopping_metric], na.rm = TRUE)

    if (k == 1) cat("\n   ",iteration_stopping_metric,
                    " = ", round(error,4), "\n", sep = "")

    if (k >= 2) {
      errPrevious <- mean(metrics[metrics$iteration == k-1,
                                  iteration_stopping_metric],
                          na.rm = TRUE)

      errImprovement <- error - errPrevious
      if (!is.na(error) & !is.na(errImprovement)) {
        percentImprove <- (errImprovement / errPrevious)
      }

      if (!is.na(errImprovement)) {
        if (percentImprove < 0) {
          cat("\n   ",iteration_stopping_metric,
              " = ", round(error,4), " (improved by ",
              round(-percentImprove*100, 3),"%)", "\n", sep = "")
        }
        else {
          cat("\n   ",iteration_stopping_metric,
              " = ", round(error,4), " (increased by ",
              round(percentImprove*100, 3),"%)", "\n", sep = "")
        }

        #print(paste0(iteration_stopping_metric,
        #            " improved by: ", round(-percentImprove*100,4),"%"))
        #running <- errImprovement < (-iteration_stopping_tolerance)
        running <- percentImprove < (-iteration_stopping_tolerance)
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

  return(list(running = running, error = error, improvement <- errImprovement))
}
