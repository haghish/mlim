#' @title Process and validate mlim settings
#' @description Validates settings and prepares runtime
#' options used by mlim.
#' @importFrom tools file_ext
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

syntaxProcessing <- function(
    data, hierarchy, preimpute, impute, matching,
    maxiter, max_models, tuning_time, cv, cpu,
    verbosity, report, save) {

  is_scalar_number <- function(x) {
    is.numeric(x) && length(x) == 1L &&
      !is.na(x) && is.finite(x)
  }

  is_whole_number <- function(x) {
    is_scalar_number(x) && x == floor(x)
  }

  fail <- function(...) {
    stop(..., call. = FALSE)
  }

  # Validate data
  # ============================================================
  if (!inherits(data, "data.frame")) {
    fail("'data' must be a data.frame.")
  }

  if (nrow(data) < 1L || ncol(data) < 1L) {
    fail("'data' must contain rows and columns.")
  }

  bad_names <- is.null(names(data)) ||
    anyNA(names(data)) || any(!nzchar(names(data)))

  if (bad_names) {
    fail("Column names must be non-empty.")
  }

  if (anyDuplicated(names(data))) {
    fail("Column names must be unique.")
  }

  # Validate hierarchy
  # ============================================================
  if (!is.null(hierarchy)) {
    valid_hierarchy <- is.character(hierarchy) &&
      length(hierarchy) > 0L && !anyNA(hierarchy) &&
      all(nzchar(hierarchy))

    if (!valid_hierarchy) {
      fail("'hierarchy' must be NULL or a character vector of column names.")
    }

    if (anyDuplicated(hierarchy)) {
      fail("'hierarchy' must not contain duplicate column names.")
    }

    missing_hierarchy <- setdiff(hierarchy, names(data))
    if (length(missing_hierarchy) > 0L) {
      fail(
        "Hierarchy variables not found in 'data': ",
        paste(missing_hierarchy, collapse = ", "), "."
      )
    }

    hierarchy_with_na <- hierarchy[vapply(
      data[hierarchy], anyNA, logical(1)
    )]

    if (length(hierarchy_with_na) > 0L) {
      fail(
        "Hierarchy variables cannot contain missing values: ",
        paste(hierarchy_with_na, collapse = ", "), "."
      )
    }
  }

  fully_missing <- names(data)[vapply(
    data, function(x) all(is.na(x)), logical(1)
  )]

  if (length(fully_missing) > 0L) {
    fail(
      "Completely missing variables cannot be imputed: ",
      paste(fully_missing, collapse = ", "), "."
    )
  }

  # Validate imputation settings
  # ============================================================
  valid_preimpute <- is.character(preimpute) &&
    length(preimpute) == 1L && !is.na(preimpute) &&
    tolower(preimpute) %in% c("rf", "mm", "random")

  if (!valid_preimpute) {
    fail("'preimpute' must be 'RF', 'mm', or 'random'.")
  }

  valid_impute <- is.character(impute) &&
    length(impute) > 0L && !anyNA(impute)

  if (!valid_impute) {
    fail("'impute' must contain an algorithm.")
  }

  algorithms <- unique(toupper(impute))
  supported <- c(
    "ELNET",
    "RF",
    "CRF",
    "GBM",
    "XGB",
    "LGBM",
    "CAT",
    "NNET",
    "SVM",
    "KNN",
    "NB",
    "ENSEMBLE"
  )
  unsupported <- setdiff(algorithms, supported)

  if (length(unsupported) > 0L) {
    fail(
      "Unsupported imputation algorithm(s): ",
      paste(unsupported, collapse = ", "), "."
    )
  }

  valid_matching <- is.logical(matching) &&
    length(matching) == 1L &&
    !is.na(matching)

  if (!valid_matching) {
    fail("'matching' must be TRUE or FALSE.")
  }

  if (!is_whole_number(maxiter) || maxiter < 1L) {
    fail("'maxiter' must be a positive integer.")
  }

  bad_max_models <- !is.null(max_models) &&
    (!is_whole_number(max_models) || max_models < 1L)

  if (bad_max_models) {
    fail("'max_models' must be NULL or a positive integer.")
  }

  base_algorithms <- setdiff(algorithms, "ENSEMBLE")

  if ("ENSEMBLE" %in% algorithms && length(base_algorithms) < 2L) {
    fail(
      "'ENSEMBLE' requires at least two additional base algorithms."
    )
  }

  if (!is.null(max_models) && max_models < length(base_algorithms)) {
    fail(
      "'max_models' must be at least the number of selected base algorithms (",
      length(base_algorithms), ")."
    )
  }

  if (!is_scalar_number(tuning_time) || tuning_time <= 0) {
    fail("'tuning_time' must be positive.")
  }

  if (!is_whole_number(cv) || cv < 5L) {
    fail("'cv' must be an integer of at least 5.")
  }

  if (!is_whole_number(cpu) || cpu < 1L) {
    fail("'cpu' must be a positive integer.")
  }

  bad_report <- !is.null(report) &&
    (!is.character(report) || length(report) != 1L ||
       is.na(report) || !nzchar(report))

  if (bad_report) {
    fail("'report' must be NULL or a file name.")
  }

  if (!is.null(save)) {
    valid_save <- is.character(save) &&
      length(save) == 1L && !is.na(save) &&
      nzchar(save)

    if (!valid_save) {
      fail("'save' must be NULL or a file name.")
    }

    if (tolower(tools::file_ext(save)) != "mlim") {
      fail("'save' must have a '.mlim' extension.")
    }
  }

  # Configure logging
  # ============================================================
  debug <- FALSE

  if (is.null(verbosity)) {
    verbose <- 0L
  }
  else {
    valid_verbosity <- is.character(verbosity) &&
      length(verbosity) == 1L && !is.na(verbosity) &&
      verbosity %in% c("warn", "info", "debug")

    if (!valid_verbosity) {
      fail("'verbosity' must be NULL, warn, info, or debug.")
    }

    verbose <- switch(
      verbosity,
      warn = 1L,
      info = 2L,
      debug = 3L
    )

    debug <- identical(verbosity, "debug")
  }

  list(
    verbose = verbose,
    debug = debug
  )
}
