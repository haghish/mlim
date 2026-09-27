#' @title normalized RMSE
#' @description calculates the normalized RMSE
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

nrmse <- function(imputed, incomplete, complete) {
  missing <- is.na(incomplete)
  index <- which(colSums(missing) > 0L)

  if (length(index) == 0L) {
    return(numeric(0))
  }

  out <- vapply(
    index,
    function(i) {
      v.na <- missing[, i]
      truth <- complete[v.na, i]
      estimate <- imputed[v.na, i]
      variance <- stats::var(truth, na.rm = TRUE)
      if (!is.finite(variance) || variance <= 0) {
        return(NA_real_)
      }

      sqrt(
        mean(
          (estimate - truth)^2,
          na.rm = TRUE
        ) / variance
      )
    },
    numeric(1L)
  )

  names(out) <- colnames(incomplete)[index]
  return(out)
}
