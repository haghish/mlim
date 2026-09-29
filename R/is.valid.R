# ----------------------------------------------------------
# is.valid
# ==========================================================
#' @title validate an object
#' @description a function to make sure the value is not NULL,
#'              NA, NaN, or Inf
#' @return locical. if TRUE, the object is valid
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
is.valid = function(x) {
  if (!is.null(x)) {
    if (is.na(x)) {
      return(FALSE)
    }
    else if (is.infinite(x)) {
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  }
  else {
    return(FALSE)
  }
}
