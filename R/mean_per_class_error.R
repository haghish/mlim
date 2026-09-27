#' @title Mean per-class error (MPCE)
#' @description Calculates the misclassification error
#' for each class and optionally returns their mean.
#' @param imputed Imputed data frame.
#' @param incomplete Original incomplete data frame.
#' @param complete Original complete data frame.
#' @param mean Logical. If TRUE, the mean error across
#' classes represented among the originally missing
#' observations is returned.
#' @return Numeric vector of class-specific errors, or a
#' single mean error when \code{mean = TRUE}.
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

mean_per_class_error <- function(
    imputed, incomplete, complete,
    mean = FALSE) {

  mpce <- NULL

  for (k in seq_len(ncol(complete))) {

    if (is.factor(complete[[k]])) {
      lvl <- levels(complete[[k]])
    }
    else {
      lvl <- unique(
        as.character(complete[[k]])
      )
    }

    for (i in lvl) {

      index <- (
        as.character(complete[[k]]) == i &
          is.na(incomplete[[k]])
      )

      if (!any(index)) {
        next
      }

      predicted <- as.character(
        imputed[[k]][index]
      )
      observed <- as.character(
        complete[[k]][index]
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

      name <- paste0(
        colnames(complete)[k],
        ":", i
      )

      mpce <- c(
        mpce,
        setNames(value, name)
      )
    }
  }

  if (mean) {

    valid <- is.finite(mpce)

    if (!any(valid)) {
      return(NA_real_)
    }

    return(mean(mpce[valid]))
  }

  return(mpce)
}
