
#' @title syntaxProcessing
#' @description extracts performance metrics from cross-validation
#' @importFrom memuse Sys.meminfo
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd


syntaxProcessing <- function(data, preimpute, impute, ram,
                             matching, miniter, maxiter, max_models,
                             tuning_time, cv, weights_column,
                             verbosity, report) {

  #if ("GBM" %in% impute) {
    #if (nrow(data) < 200) stop("too few rows... use 'ELNET' or 'DRF' instead")
  #}

  # default values
  verbose <- NULL
  debug   <- FALSE
  min_ram <- NULL
  max_ram <- NULL

  stopifnot(
    #"'data' is not a data.frame" = is.data.frame(data) | inherits(data, "mlim"),
    #"'data' has no observations" = dim(data) >= 1, #not applicable to mlim object
    #"'formula' should be a formula!" = inherits(formula, "formula"),
    #length(formula <- as.character(formula)) == 3L,
    #"'max_models' must be a positive integer equal or more than 1" = max_models >= 1,
    #"'tuning_time' must be a positive integer" = tuning_time >= 2,
    "'cv' must be a positive integer equal or more than 10" = cv >= 10
  )

  #if (miniter < 2 & preimpute == "iterate") stop("'miniter' must not be less than 2")
  #if ( maxiter < 2 & preimpute == "iterate") stop("'maxiter' must not be less than 2")

  if (!is.null(weights_column)) {
    stopifnot(
      "'weights_column' must have equal length with the data.frame" = length(weights_column) == nrow(data),
      "'weights_column' must not have any missing observations" = !anyNA(weights_column)
    )
  }


  if (!is.null(ram)) {
    if (!is.numeric(ram)) stop("'ram' must be an integer, specifying amount of RAM in Gigabytes")
    min_ram <- paste0(ram - 1, "G")
    max_ram <- paste0(ram, "G")
  }
  else {
    ram <- floor(as.numeric(memuse::Sys.meminfo()$freeram)*9.31*1e-10)
    min_ram <- paste0(ram - 1, "G")
    max_ram <- paste0(ram, "G")
  }

  if ("StackEnsemble" %in% impute) {
    keep_cross_validation_predictions <- TRUE
  }
  else {
    keep_cross_validation_predictions <- FALSE
  }

  # define logging levels and debugging
  if (is.null(verbosity)) verbose <- 0
  else if (verbosity == "warn") verbose <- 1
  else if (verbosity == "info") verbose <- 2
  else if (verbosity == "debug") {
    verbose <- 3
    debug <- TRUE
  }

  return(list(min_ram=min_ram,
              max_ram=max_ram,
              keep_cross_validation_predictions=keep_cross_validation_predictions,
              verbose=verbose,
              debug=debug))
}
