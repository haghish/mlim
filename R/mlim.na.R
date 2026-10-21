#' @title add stratified/unstratified artificial missing observations
#' @description to examine the performance of imputation algorithms, artificial
#'              missing data are added to datasets and then imputed, to compare
#'              the original observations with the imputed values. this function
#'              can add stratified or unstratified artificial missing data.
#'              stratification is useful when categorical or ordinal variables
#'              are imbalanced.
#' @param x data.frame. x must be strictly a data.frame and any other
#'          data.table classes will be rejected
#' @param p proportion of missingness to be added to the data
#' @param stratify logical. if TRUE, stratified sampling will be
#'                 carried out, when adding NA values to 'factor' variables
#'                 (either ordered or unordered). this feature makes evaluation
#'                 of missing data imputation algorithms more fair, especially
#'                 when the factor levels are imbalanced.
#' @param classes character vector, specifying the variable classes that should
#'                be selected for adding NA values. the default value is NULL,
#'                meaning all variables can receive NA values. use classes
#'                such as c('numeric', 'ordered') to restrict selection.
#' @param seed integer. random seed for reproducibility (recommended)
#' @param mechanism missingness mechanism: "mcar" (default) or "mar".
#' @param mar named numeric vector or coefficient matrix for MAR. matrix rows
#'            identify incomplete variables; columns identify predictors of
#'            missingness. a vector uses its named predictors for all other
#'            variables. rows are processed in order; previously masked values
#'            are not used. ignored when mechanism = "mcar".
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
#'
#' # MAR: remove predictor values based on the fully observed outcome
#' dat <- data.frame(v1 = rnorm(100), v2 = rnorm(100), outcome = rnorm(100))
#' mlim.na(dat, p = 0.15, mechanism = "mar", mar = c(outcome = 1))
#' }
#' @return data.frame or atomic vector
#' @export

mlim.na <- function(x, p = 0.1, stratify = FALSE, classes = NULL,
                    seed = NULL, mechanism = "mcar", mar = NULL) {

  # Syntax processing
  # ------------------------------------------------------------
  stopifnot(
    "'p' should be between 0 and 1" = p >= 0 & p <= 1,
    "'x' type is not recognized" = is.atomic(x) || is.data.frame(x),
    "class of 'x' must be strictly data.frame" =
      !any(class(x) %in% c("tbl", "tbl_df"))
  )

  mechanism <- match.arg(mechanism, c("mcar", "mar"))

  # set the seed for reproducibility
  if (!is.null(seed)) set.seed(seed)

  # Add MAR missingness using observed predictors of missingness.
  if (mechanism == "mar") {
    if (!is.data.frame(x) || is.null(mar)) {
      stop("MAR requires a data.frame and a named vector or matrix.")
    }
    if (stratify) stop("'stratify' is only available for MCAR.")

    # A vector applies the same model to all variables except its drivers.
    if (is.null(dim(mar))) {
      vars <- setdiff(names(x), names(mar))
      mar <- matrix(rep(mar, each = length(vars)), nrow = length(vars),
                    dimnames = list(vars, names(mar)))
    }

    vars <- rownames(mar)
    if (is.null(vars) || is.null(colnames(mar))) {
      stop("MAR matrix rows and columns must have variable names.")
    }
    if (!is.null(classes)) {
      vars <- vars[vapply(x[vars], function(z) {
        class(z)[1] %in% classes
      }, logical(1))]
    }
    if (p == 0) return(x)
    if (p == 1) {
      for (v in vars) x[[v]][] <- NA
      return(x)
    }

    for (i in seq_along(vars)) {
      v <- vars[i]
      b <- mar[v, ]
      drivers <- names(b)[b != 0]

      # A MAR driver must remain observed, or have been processed earlier.
      if (any(drivers %in% vars[i:length(vars)])) {
        stop("MAR cannot depend on current or later missing variables.")
      }

      eta <- numeric(nrow(x))
      for (d in drivers) {
        observed <- !is.na(x[[d]])
        eta[observed] <- eta[observed] +
          b[d] * as.numeric(scale(x[[d]][observed]))
      }

      # Calibrate the expected missingness rate for each variable to p.
      alpha <- stats::uniroot(
        function(a) mean(stats::plogis(a + eta)) - p,
        interval = c(-40, 40)
      )$root
      x[[v]][stats::runif(nrow(x)) < stats::plogis(alpha + eta)] <- NA
    }
    return(x)
  }

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
        if (class(x[, i, drop = TRUE])[1] %in% classes) {
          x[, i] <- addNA(x[, i], p, stratify = stratify)
        }
      }
    }
  }

  return(x)
}


