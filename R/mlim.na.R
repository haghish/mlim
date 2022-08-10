#' @title syntaxProcessing
#' @description extracts performance metrics from cross-validation
#' @param x data.frame. x must be strictly a data.frame and any other
#'          data.table classes will be rejected
#' @param p percentage of missingness to be added to the data
#' @param stratify logical. if TRUE (default), stratified sampling will be
#'                 carried out, when adding NA values to 'factor' variables
#'                 (either ordered or unordered). this feature makes evaluation
#'                 of missing data imputation algorithms more fair, especially
#'                 when the factor levels are imbalanced.
#' @param classes character vector, specifying the variable classes that should
#'                be selected for adding NA values. the default value is NULL,
#'                meaning all variables will receive NA values with probability of 'p'.
#'                however, if you wish to add NA values only to a specific classes, e.g.
#'                'numeric' variables or 'ordered' factors, specify them in this argument.
#'                e.g. write "classes = c('numeric', 'ordered')" if you wish to add NAs
#'                only to numeric and ordered factors.
#' @param seed integer. a random seed number for reproducing the result (recommended)
#' @author E. F. Haghish
#' @examples
#'
#' \dontrun{
#' # adding stratified NA to an atomic vector
#' x <- as.factor(c(rep("M", 100), rep("F", 900)))
#' table(mlim.na(x, p=.5, stratify = TRUE))
#'
#' # adding unstratified NAs to all variables of a data.frame
#' data(iris)
#' mlim.na(iris, p=0.5, stratify = FALSE, seed = 1)
#'
#' # or add stratified NAs only to factor variables, ignoring other variables
#' mlim.na(iris, p=0.5, stratify = TRUE, classes = "factor", seed = 1)
#'
#' # or add NAs to numeric variables
#' mlim.na(iris, p=0.5, classes = "numeric", seed = 1)
#' }
#' @export

mlim.na <- function(x, p = 0.1, stratify=FALSE, classes=NULL, seed = NULL) {

  # Syntax processing
  # ------------------------------------------------------------
  stopifnot(
    "'p' should be between 0 and 1" = p >= 0 & p <= 1,
    "'x' type is not recognized" = is.atomic(x) || is.data.frame(x),
    "clas of 'x' must be strictly data.frame" = ! class(x) %in% c("tbl", "tbl_df")
  )

  # set the seed for reproducibility
  if (!is.null(seed)) set.seed(seed)

  # if 'x' is not a dataframe:
  if (is.atomic(x)) {
    x <- addNA(x, p, stratify = stratify)
    return(x)
  }
  else {
    for (i in colnames(x)) {
      if (is.null(classes)) x[, i] <- addNA(x[, i], p, stratify = stratify)
      else {
        # force drop in case 'x' is not a data.frame
        if (class(x[, i, drop=TRUE])[1] %in% classes) x[, i] <- addNA(x[, i], p, stratify = stratify)
      }
    }
  }

  return(x)
}


