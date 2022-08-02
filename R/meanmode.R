#' @title mean and mode imputation
#' @description Compute the missing values with mean and mode replacement
#' @param data A data frame with dummies or numeric variables.
#' @return imputed dataset
#' @author E. F. Haghish, Maintainer: \email{haghish@@uio.no}
#' @noRd
#' @keywords Internal

meanmode <- function (data) {

  Mode <- function(x) {
    xtab <- table(x)
    xmode <- names(which(xtab == max(xtab)))
    return(xmode[1])
  }

  values <- sapply(data, function(x) {
    if (class(x) %in% c("character", "factor"))
      Mode(x)
    else if (class(x) %in% c("numeric", "integer"))
      median(x, na.rm = TRUE)

  }, simplify = FALSE)

  #impute the values ??? improve it in the future release and make it more secure
  for (i in 1:length(values)) {
    data[is.na(data[,i]), i] <- values[i]
  }

  return(data)
}



