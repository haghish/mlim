#' @title getDigits
#' @description extracts number of digits
#' @author E. F. Haghish
#' @return integer
#' @keywords Internal
#' @noRd

getDigits <- function(x) {
  result <- floor(log10(abs(x)))
  result[!is.finite(result)] = 0
  return(abs(result))
}
