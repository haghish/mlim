#' @title save mlim object
#' @description saves mlim object in feather format
#' @importFrom arrow write_feather
#' @param mlimobject list of class mlim
#' @param save character specifying file name of the mlim object
#' @return nothing
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

mlim.save <- function(x, save) {
  class(x) <- "mlim"
  result <- data.frame()
  attr(result, "mlim") <- x
  arrow::write_feather(x=result, sink=save)
}
