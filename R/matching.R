
#' @title match imputed missing observations to non-missing values
#' @description each imputed numeric missing value is replaced with the nearest
#'              non-missing value. this option is particularly
#'              recommended when ordinal variables are imputed as a numeric
#'              variables.
#' @param imputed numeric vector of imputed missing values
#' @param nonMiss numeric vector of non-missing values
#' @return numeric vector of the imputed values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

matching <- function(imputed, nonMiss, md.log) {

  if (!is.null(imputed) & !is.null(nonMiss)) {
    if (is.numeric(imputed) & is.numeric(nonMiss)) {
      # get the unique values
      unqImputed <- unique(imputed)
      unqNonMiss <- unique(nonMiss[!is.na(nonMiss)])

      # avoid imputed values that are already in nnMiss set
      unqImputed <- setdiff(unqImputed, unqNonMiss)

      # index the unique values and replace them in the vectors
      for (i in unqImputed) {
        nearest <- which.min(abs(unqNonMiss - i))
        index <- which(imputed == i)
        imputed[index] <- nearest
      }

      md.log("matching successul!")

      return(imputed)
    }
  }
}


#nonMiss <- c(1:20, 19:1)
#imputed <- c(11.5, 12.2, 11.51, 14.1, -1, 49, 20, 1, 4)
#matching(imputed, nonMiss)


