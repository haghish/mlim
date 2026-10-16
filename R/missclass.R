#' @title Misclassification error
#' @description Calculates the proportion of incorrect imputations among
#'   originally missing cells, separately for each variable containing NAs.
#'   Missing values that remain after imputation count as errors.
#' @param imputed Data frame containing the imputed values.
#' @param incomplete Original data frame containing missing values.
#' @param complete Original complete data frame used as the reference.
#' @param rename Logical. If TRUE, name the results using variable names.
#' @return A numeric vector with misclassification rates between 0 and 1,
#'   for variables with originally missing values.
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
missclass <- function(imputed, incomplete, complete, rename = TRUE) {
  if (!all(vapply(list(imputed, incomplete, complete), is.data.frame, logical(1))))
    stop("'imputed', 'incomplete', and 'complete' must be data frames")
  if (!identical(dim(imputed), dim(incomplete)) || !identical(dim(complete), dim(incomplete)) ||
      !identical(names(imputed), names(incomplete)) || !identical(names(complete), names(incomplete)))
    stop("All three data frames must have the same dimensions and variable order")
  if (anyNA(complete)) stop("'complete' must not contain missing values")
  if (!is.logical(rename) || length(rename) != 1L || is.na(rename))
    stop("'rename' must be TRUE or FALSE")

  index <- which(colSums(is.na(incomplete)) > 0L)
  classerror <- vapply(index, function(i) {
    missing <- is.na(incomplete[[i]])
    predicted <- as.character(imputed[[i]][missing])
    actual <- as.character(complete[[i]][missing])
    mean(is.na(predicted) | predicted != actual)
  }, numeric(1))

  if (rename) names(classerror) <- names(incomplete)[index]
  classerror
}
