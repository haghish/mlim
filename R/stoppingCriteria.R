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
                             iteration_stopping_tolerance) {

  # keep running unless...
  running <- TRUE
  error <- NA
  errImprovement <- NA



  # there has been no (or too little) improvement
  # ------------------------------------------------------------
  if (running) {
    if (k >= 2) {
      error <- mean(metrics[metrics$iteration == k,
                            iteration_stopping_metric], na.rm = TRUE)

      errImprovement <- error - mean(metrics[metrics$iteration == k-1,
                                             iteration_stopping_metric],
                                     na.rm = TRUE)

      if (!is.na(error)) {
        print(paste("err:", error))
        running <- errImprovement < (-iteration_stopping_tolerance)
      }
    }
  }

  # maximum iteration has been reached
  # ------------------------------------------------------------
  if (k == maxiter) running <- FALSE

  return(list(running = running, error = error, improvement <- errImprovement))
}
