
#' @title match imputed ordinal missing observations to non-missing values
#' @description Replaces each imputed ordinal value with
#' the nearest valid ordinal level. Ties are resolved in
#' favor of the lower level.
#' @param imputed numeric vector of imputed missing values
#' @param nonMiss numeric vector of non-missing values
#' @return numeric vector of the imputed values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

matching <- function(imputed, support) {

  if (!is.numeric(imputed)) {
    stop("'imputed' must be numeric.", call. = FALSE)
  }

  if (!is.numeric(support) || length(support) < 1L) {
    stop(
      "'support' must contain valid ordinal levels.",
      call. = FALSE
    )
  }

  support <- sort(unique(support))

  if (anyNA(support) || any(!is.finite(support))) {
    stop(
      "'support' must contain finite values only.",
      call. = FALSE
    )
  }

  observed <- !is.na(imputed)
  if (any(!is.finite(imputed[observed]))) {
    stop(
      "'imputed' contains non-finite values.",
      call. = FALSE
    )
  }

  for (i in which(observed)) {
    distance <- abs(support - imputed[i])
    nearest <- support[distance == min(distance)]
    imputed[i] <- min(nearest)
  }

  return(imputed)
}
