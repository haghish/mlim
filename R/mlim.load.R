#' @title loads mlim object from mlim file
#' @description loads mlim object and creates a list
#' @importFrom arrow read_feather
#' @param file path to the mlim file on the disk
#' @return object of class mlim
#' @author E. F. Haghish
#' @export

mlim.load <- function(file) {
  if (inherits(file, "character")) {
    result <- arrow::read_feather(file=file)
    result <- attributes(result)$mlim
    if (!inherits(result, "mlim")) stop("the loaded object is not prepared for mlim version 0.2 or higher")
    return(result)
  }
  else {
    stop("specify the path to the mlim object file on the disk")
  }
}
