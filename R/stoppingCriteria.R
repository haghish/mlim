#' @title iteration stopping criteria
#' @description evaluates the stopping criteria
#' @param metrics estimated error from CV
#' @param k iteration round
#' @param maxiter maximum number of iterations
#' @param error_metric character. stopping metric for the iteration. default is "RMSE"
#' @return list containing the running status and current best error
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

stoppingCriteria <- function(metrics, k, maxiter, error_metric) {

  # ------------------------------------------------------------
  # Identify accepted variable updates in the current iteration
  #
  # iterate() sets the error metric to NA when a newly fitted model
  # does not improve the best previous model beyond the tolerance.
  # Therefore, the imputation should continue as long as at least
  # one variable was improved in the current iteration.
  # ============================================================
  current <- metrics[metrics$iteration == k, error_metric]
  improved <- any(!is.na(current))


  # ------------------------------------------------------------
  # Calculate the error of the current best imputed dataset
  #
  # Rejected models are stored with NA for the stopping metric.
  # The current imputed dataset therefore corresponds to the best
  # accepted model reached for each variable across the iterations.
  # ============================================================
  variables <- unique(metrics$variable)
  best_error <- vapply(variables, function(i) {
    x <- metrics[metrics$variable == i, error_metric]
    x <- x[!is.na(x)]

    if (length(x) == 0L) NA_real_
    else min(x)
  }, numeric(1))

  if (all(is.na(best_error))) {
    error <- NA_real_
  }
  else {
    error <- mean(best_error, na.rm = TRUE)
  }


  # ------------------------------------------------------------
  # Continue only if at least one variable improved and the maximum
  # number of iterations has not been reached.
  # ============================================================
  running <- improved && k < maxiter


  # ------------------------------------------------------------
  # Warn when the maximum number of iterations is reached while at
  # least one variable is still improving.
  # ============================================================
  if (k >= maxiter && improved) {
    warning("the imputation could be further improved by increasing number of iterations")
  }


  return(list(running = running,
              error = error))
}
