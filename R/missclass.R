#' @title missclassification error
#' @description calculates the missclassification rate for each variable
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
missclass <- function(imputed, incomplete, complete, rename = TRUE){
  classerror <- NULL
  mis <- as.data.frame(is.na(incomplete))
  index <- which(colSums(mis) > 0)
  for (i in index) {
    missclass <- sum(as.character(as.matrix(imputed[,i])) != as.character(as.matrix(complete[,i])))
    NAs <- sum(is.na(incomplete[, i]))
    classerror <- c(classerror, (missclass / NAs))
  }
  if (rename) names(classerror) <- colnames(incomplete)[index]
  return(classerror)
}
