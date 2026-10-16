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
                             md.log) {

  # Keep running unless the specified stopping criterion is reached.
  running <- TRUE
  error <- NA
  errImprovement <- NA

  # Variable-wise stopping: iterate() decides whether to accept each update.
  # A finite metric in the current iteration indicates an accepted update;
  # rejected updates are recorded as Inf in the metrics table.
  if (method == "varwise_NA") {
    if (running) {
      current <- metrics[metrics$iteration == k, error_metric]
      current <- current[is.finite(current)]
      error <- if (length(current)) mean(current) else NA_real_
      if (is.na(error)) running <- FALSE
    }
  }

  # Overall iteration-wise stopping based on relative RMSE improvement.
  if (method == "iteration_RMSE") {
    if (running) {
      error <- mean(metrics[metrics$iteration == k,
                            error_metric], na.rm = TRUE)

      if (k == 1) message("\n   ", error_metric,
                          " = ", round(error, 4), "\n", sep = "")

      if (k >= 2) {
        available <- !is.na(metrics[metrics$iteration == k, error_metric])
        errPrevious <- mean(metrics[metrics$iteration == k - 1 & available,
                                    error_metric], na.rm = TRUE)

        errImprovement <- error - errPrevious
        if (!is.na(error) & !is.na(errImprovement)) {
          percentImprove <- errImprovement / errPrevious
        }

        if (!is.na(errImprovement)) {
          if (percentImprove < 0) {
            message("\n   ", error_metric,
                    " = ", round(error, 4), " (improved by ",
                    round(-percentImprove * 100, 3), "%)", "\n", sep = "")
          } else {
            message("\n   ", error_metric,
                    " = ", round(error, 4), " (increased by ",
                    round(percentImprove * 100, 3), "%)", "\n", sep = "")
          }
          running <- percentImprove < -tolerance
        }
      }
    }
  }

  # Reaching the user-specified iteration limit is a normal stopping
  # condition, NOT evidence that another iteration would improve imputation.
  # Earlier versions warned whenever k == maxiter and any finite metric was
  # present, including runs that deliberately specified maxiter = 1 or 2.
  # Preserve the stop behavior without issuing a misleading warning.
  if (k >= maxiter) running <- FALSE

  return(list(running = running,
              error = error,
              vars2impute = vars2impute,
              improvement = errImprovement))
}
