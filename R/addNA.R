#' @title add NA in a vector
#' @description generates NA and replaces observed values
#'              of a vector with NA
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

addNA <- function(x, p, stratify = FALSE) {

  if (stratify && "factor" %in% class(x)) {

    levs <- levels(x)

    for (l in levs) {

      index <- which(!is.na(x) & x == l)
      len <- length(index)
      nmiss <- round(p * len)

      if (nmiss > 0L) {
        x[sample(index, nmiss)] <- NA
      }
    }
  }
  else {

    index <- which(!is.na(x))
    len <- length(index)
    nmiss <- round(p * len)

    if (nmiss > 0L) {
      x[sample(index, nmiss)] <- NA
    }
  }

  return(x)
}
