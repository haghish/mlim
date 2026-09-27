#' @title Misclassification error
#' @description Calculates the misclassification rate
#' for each variable at originally missing positions.
#' @param imputed Imputed data frame.
#' @param incomplete Original incomplete data frame.
#' @param complete Original complete data frame.
#' @param rename Logical. If TRUE, returned values are
#' named using the corresponding variable names.
#' @return Numeric vector of misclassification rates.
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

missclass <- function(
    imputed, incomplete, complete,
    rename = TRUE) {

  classerror <- NULL
  mis <- is.na(incomplete)

  index <- which(
    colSums(mis) > 0L
  )

  for (i in index) {

    v.na <- mis[, i]

    predicted <- as.character(
      imputed[v.na, i]
    )
    observed <- as.character(
      complete[v.na, i]
    )

    if (anyNA(predicted) ||
        anyNA(observed)) {
      value <- NA_real_
    }
    else {
      value <- mean(
        predicted != observed
      )
    }

    classerror <- c(
      classerror, value
    )
  }

  if (rename &&
      length(classerror) > 0L) {
    names(classerror) <-
      colnames(incomplete)[index]
  }

  return(classerror)
}
