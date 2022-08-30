#' @title process ... arguments
#' @description extract values from '...' argument and  pass them to the scalars
#' @param name character. the name of the object of interest
#' @param ... arguments list
#' @return value of the object of interest
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
threeDots <- function(name, ..., default){
  dotList <- list(...)
  dotName <- names(dotList)
  if (name %in% dotName) {
    return(dotList[[name]])
  }
  else return(default)
}
