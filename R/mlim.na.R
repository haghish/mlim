
#' @title add NA in a vector
#' @description generates NA and replaces the actual values of a vector
#'              with NA
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
addNA <- function(x, p) {
  len <- length(x)
  # sample without replacement
  x[sample(len, round(p * len))] <- NA
  return(x)
}

#' @title syntaxProcessing
#' @description extracts performance metrics from cross-validation
#' @author E. F. Haghish
#' @export

mlim.na <- function(x, p = 0.1, classes=NULL, seed = NULL) {

  # Syntax processing
  # ------------------------------------------------------------
  stopifnot(
    "'p' should be between 0 and 1" = p >= 0 & p <= 1,
    "'x' type is not recognized" = is.atomic(x) || is.data.frame(x),
    "'x' must be data.frame" = ! class(x) %in% c("tbl", "tbl_df")
  )

  # set the seed for reproducibility
  if (!is.null(seed)) set.seed(seed)

  # if 'x' is not a dataframe:
  if (is.atomic(x)) {
    x <- addNA(x, p)
    return(x)
  }
  else {
    for (i in colnames(x)) {
      if (is.null(classes)) x[, i] <- addNA(x[, i], p)
      else {
        # force drop in case 'x' is not a data.frame
        if (class(x[, i, drop=TRUE])[1] %in% classes) x[, i] <- addNA(x[, i], p)
      }
    }
  }

  return(x)
}

#print(mlim.na(iris, p=.5, classes = "numeric"))
#print(mlim.na(df, p=.99, seed=2022, classes="factor"))



