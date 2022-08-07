
#' @title syntaxProcessing
#' @description extracts performance metrics from cross-validation
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd


syntaxProcessing <- function(data, preimpute, include_algos,  matching, miniter, maxiter, max_models,
                             max_model_runtime_secs, nfolds, weights_column,
                             md.log) {

  if ("GBM" %in% include_algos) {
    if (nrow(data) < 200) stop("too few rows... use 'ELNET' or 'DRF' instead")
  }

  stopifnot(
    "'data' is not a data.frame" = is.data.frame(data),
    "'data' has no observations" = dim(data) >= 1,
    #"'formula' should be a formula!" = inherits(formula, "formula"),
    #length(formula <- as.character(formula)) == 3L,
    #"'max_models' must be a positive integer equal or more than 1" = max_models >= 1,
    #"'max_model_runtime_secs' must be a positive integer" = max_model_runtime_secs >= 2,
    "'nfolds' must be a positive integer equal or more than 10" = nfolds >= 10
  )

  #if (miniter < 2 & preimpute == "iterate") stop("'miniter' must not be less than 2")
  #if ( maxiter < 2 & preimpute == "iterate") stop("'maxiter' must not be less than 2")

  if (!is.null(weights_column)) {
    stopifnot(
      "'weights_column' must have equal length with the data.frame" = length(weights_column) == nrow(data),
      "'weights_column' must not have any missing observations" = !anyNA(weights_column)
    )
  }

  if (!is.null(md.log)) md.log("Syntax Processing was successful")
}
