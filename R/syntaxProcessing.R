#' @title Process and validate mlim settings
#' @description Validates settings and prepares runtime options used by mlim.
#' @importFrom tools file_ext
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
syntaxProcessing <- function(data, hierarchy, preimpute, impute, matching,
                             maxiter, max_models, tuning_time, cv, cpu,
                             verbosity, report, save) {
  scalar_number <- function(x) is.numeric(x) && length(x) == 1L &&
    !is.na(x) && is.finite(x)
  whole_number <- function(x) scalar_number(x) && x == floor(x)
  fail <- function(...) stop(..., call. = FALSE)
  if (!inherits(data, "data.frame") || !nrow(data) || !ncol(data))
    fail("'data' must be a non-empty data.frame.")
  if (is.null(names(data)) || anyNA(names(data)) ||
      any(!nzchar(names(data))) || anyDuplicated(names(data)))
    fail("Column names must be non-empty and unique.")
  if (!is.null(hierarchy)) {
    if (!is.character(hierarchy) || !length(hierarchy) ||
        anyNA(hierarchy) || any(!nzchar(hierarchy)) || anyDuplicated(hierarchy))
      fail("'hierarchy' must be a non-empty character vector of unique names.")
    if (!all(hierarchy %in% names(data)))
      fail("Missing hierarchy columns: ", paste(setdiff(hierarchy, names(data)), collapse = ", "))
    if (anyNA(data[hierarchy])) fail("Hierarchy variables cannot contain missing values.")
  }
  fully_missing <- names(data)[vapply(data, function(x) all(is.na(x)), logical(1))]
  if (length(fully_missing)) fail("Completely missing variables cannot be imputed: ",
                                   paste(fully_missing, collapse = ", "))
  if (!is.character(preimpute) || length(preimpute) != 1L ||
      is.na(preimpute) || !tolower(preimpute) %in% c("rf", "mm", "random"))
    fail("'preimpute' must be 'rf', 'mm', or 'random'.")
  if (!is.character(impute) || !length(impute) || anyNA(impute))
    fail("'impute' must be a nonempty character vector.")
  algorithms <- unique(toupper(impute))
  supported <- c("ELNET", "RF", "CRF", "GBM", "XGB", "CAT", "NNET",
                 "SVM", "KNN", "NB", "ENSEMBLE", "HML")
  unsupported <- setdiff(algorithms, supported)
  if (length(unsupported)) fail("Unsupported algorithm(s): ", paste(unsupported, collapse = ", "))
  if ("HML" %in% algorithms) {
    if (is.null(hierarchy)) fail("The HML engine requires 'hierarchy'.")
    # Other learners currently use ordinary row-wise CV, whereas HML uses
    # blocked-cluster CV. Mixing them would compare INCOMPARABLE CV risks.
    if (length(algorithms) != 1L)
      fail("HML must be used alone until all learners share a comparable multilevel CV design.")
    if (!requireNamespace("gpboost", quietly = TRUE))
      fail("HML requires 'gpboost'; install this optional package first.")
  }
  if (!is.logical(matching) || length(matching) != 1L || is.na(matching))
    fail("'matching' must be TRUE or FALSE.")
  if (!whole_number(maxiter) || maxiter < 1L) fail("'maxiter' must be a positive integer.")
  if (!is.null(max_models) && (!whole_number(max_models) || max_models < 1L))
    fail("'max_models' must be NULL or a positive integer.")
  base_algorithms <- setdiff(algorithms, "ENSEMBLE")
  if ("ENSEMBLE" %in% algorithms && length(base_algorithms) < 2L)
    fail("'ENSEMBLE' requires at least two additional base algorithms.")
  if (!is.null(max_models) && max_models < length(base_algorithms))
    fail("'max_models' must be at least the number of requested base algorithms.")
  if (!scalar_number(tuning_time) || tuning_time <= 0)
    fail("'tuning_time' must be a positive finite number.")
  if (!whole_number(cv) || cv < 5L) fail("'cv' must be an integer of at least five.")
  if (!whole_number(cpu) || cpu < 1L) fail("'cpu' must be a positive integer.")
  if (!is.null(report) && (!is.character(report) || length(report) != 1L ||
                          is.na(report) || !nzchar(report)))
    fail("'report' must be NULL or a filename.")
  if (!is.null(save)) {
    if (!is.character(save) || length(save) != 1L || is.na(save) || !nzchar(save))
      fail("'save' must be NULL or a filename.")
    if (tolower(tools::file_ext(save)) != "mlim")
      fail("'save' must have a '.mlim' extension.")
  }
  debug <- FALSE
  if (is.null(verbosity)) {
    verbose <- 0L
  } else {
    if (!is.character(verbosity) || length(verbosity) != 1L ||
        is.na(verbosity) || !verbosity %in% c("warn", "info", "debug"))
      fail("'verbosity' must be NULL, 'warn', 'info', or 'debug'.")
    verbose <- switch(verbosity, warn = 1L, info = 2L, debug = 3L)
    debug <- identical(verbosity, "debug")
  }
  list(verbose = verbose, debug = debug)
}
