#' @title java server interaction
#' @description the function provides commands to interact with the java
#'              server
#' @param action character
#' @return character (java command)
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
javaServer <- function(action){
  result <- NULL
  if (action == "flush") result <- "h2o:::.h2o.garbageCollect()"
  return(result)
}
