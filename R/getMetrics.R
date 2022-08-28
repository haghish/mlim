# ----------------------------------------------------------
# getMetrics
# ==========================================================
#' @title retreives metrics data
#' @description a function to retreive the metrics data from
#'              datasets preimputed with mlim
#' @param preimputed.data data.frame previously imputed by mlim or
#'                        other programs
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
getMetrics = function(preimputed.data) {
  metrics <- attributes(preimputed.data)$metrics
  if (!is.null(metrics)) {
    unc <- unique(metrics$variable)
    maxmetrics <- max(metrics$iteration, na.rm = TRUE)
    for (i in unc) {
      metrics[(metrics$iteration == maxmetrics & metrics$variable == i),
              "RMSE"] <- min(metrics$RMSE[metrics$variable == i], na.rm = TRUE)
    }
    return(metrics)
  }
  else return(NULL)
}
