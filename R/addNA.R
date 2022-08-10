#' @title add NA in a vector
#' @description generates NA and replaces the actual values of a vector
#'              with NA
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
addNA <- function(x, p, stratify=FALSE) {

  if (stratify & "factor" %in% class(x)) {
    levs <- levels(x)
    for (l in levs) {
      index <- which(x == l)
      len <- length(index)
      x[index][sample(len, round(p * len))] <- NA
    }
  }
  else {
    len <- length(x)
    x[sample(len, round(p * len))] <- NA  # sample without replacement
  }
  return(x)
}
