#' @title Median and mode preimputation
#' @description Replaces missing values with the median for numeric variables
#'              and the mode for categorical variables. This function is used
#'              to initialize missing values before the iterative imputation.
#' @param data A data frame containing missing values
#' @importFrom stats median
#' @return median/mode imputed dataset
#' @author E. F. Haghish, Maintainer: \email{haghish@@uib.no}
#' @noRd
#' @keywords Internal

#' @title Median and mode preimputation
#' @description Replaces missing values with the median for numeric variables
#'              and the mode for categorical variables. This function is used
#'              to initialize missing values before the iterative imputation.
#' @param data A data frame containing numeric, integer, factor, ordered factor,
#'             or character variables.
#' @return A data frame with missing values replaced by median or mode values.
#' @author E. F. Haghish
#' @noRd
#' @keywords Internal

medianmode <- function(data) {

  Mode <- function(x) {
    observed <- x[!is.na(x)]
    if (length(observed) == 0) return(NA)
    xtab <- table(observed)
    mode <- names(xtab)[which.max(xtab)]
    return(mode)
  }

  for (i in seq_along(data)) {
    missing <- is.na(data[[i]])
    if (!any(missing)) next
    if (is.factor(data[[i]]) || is.character(data[[i]])) {
      value <- Mode(data[[i]])
      data[[i]][missing] <- value
    }

    else if (is.numeric(data[[i]])) {
      value <- stats::median(data[[i]], na.rm = TRUE)
      data[[i]][missing] <- value
    }
  }

  return(data)
}


