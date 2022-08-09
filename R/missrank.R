#' @title miss ranking error
#' @description calculates the MAE or missranking, devided by N-1 ordered categories
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
missrank <- function(imputed, incomplete, complete){
  MAE <- NULL
  mis <- as.data.frame(is.na(incomplete))
  index <- which(colSums(mis) > 0)
  for (i in index) {
    imputed[,i] <- as.numeric(imputed[,i])
    incomplete[,i] <- as.numeric(incomplete[,i])
    complete[,i] <- as.numeric(complete[,i])
    v.na <- is.na(incomplete[, i])
    MAE <- c(MAE, (mean(abs(imputed[v.na,i] - complete[v.na,i])) /
                     (length(unique(complete[,i]))-1)
    ))
  }
  names(MAE) <- colnames(incomplete)[index]
  return(MAE)
}
