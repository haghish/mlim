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
                             metrics, k,
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
    print(paste("Iteration error:", error))

    if (k >= 2) {
      errPrevious <- mean(metrics[metrics$iteration == k-1,
                                  iteration_stopping_metric],
                          na.rm = TRUE)
      errImprovement <- error - errPrevious
      if (!is.na(error) & !is.na(errImprovement)) {
        percentImprove <- (errImprovement / errPrevious)
      }

      if (!is.na(errImprovement)) {
        print(paste0(iteration_stopping_metric,
                    " improved by: ", round(-percentImprove*100,4),"%"))
        #running <- errImprovement < (-iteration_stopping_tolerance)
        running <- percentImprove < (-iteration_stopping_tolerance)
      }
    }
  }

  # maximum iteration has been reached
  # ------------------------------------------------------------
  if (k == maxiter) running <- FALSE

  return(list(running = running, error = error, improvement <- errImprovement))
}
