#' @title normalized RMSE
#' @description calculates the normalized RMSE
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
nrmse <- function(imputed, incomplete, complete){
  nrmse <- NULL
  mis <- as.data.frame(is.na(incomplete))
  index <- which(colSums(mis) > 0)
  for (i in index) {
    v.na <- is.na(incomplete[, i])
    nrmse <- c(nrmse,
               sqrt(mean((imputed[v.na,i] - complete[v.na,i])^{2}) /
                      stats::var(complete[v.na,i]))
    )

  }
  names(nrmse) <- colnames(incomplete)[index]
  return(nrmse)
  #sqrt(mean((imputed[mis] - complete[mis])^{2}) / stats::var(complete[mis]))
}
