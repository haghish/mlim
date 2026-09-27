# ----------------------------------------------------------
# is.valid
# ==========================================================
#' @title Validate an object
#' @description Checks whether an object is non-empty and does not
#'              contain missing or non-finite numeric values.
#' @return Logical. TRUE if the object is valid.
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

is.valid <- function(x) {

  if (is.null(x) || length(x) == 0L) {
    return(FALSE)
  }

  if (anyNA(x)) {
    return(FALSE)
  }

  if (is.numeric(x) && any(!is.finite(x))) {
    return(FALSE)
  }

  return(TRUE)
}
