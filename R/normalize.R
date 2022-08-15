#' @title normalize
#' @description normalizes a variable, scaling it to range from 0 to 1
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

normalize <- function(x){
  return(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
}
