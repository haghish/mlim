#' @title Process and validate mlim settings
#' @description Validates settings and prepares runtime
#' options used by mlim.
#' @importFrom memuse Sys.meminfo
#' @importFrom tools file_ext
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

syntaxProcessing <- function(
    data, preimpute, impute, ram, matching,
    maxiter, max_models, tuning_time, cv,
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

  valid_matching <- identical(matching, FALSE) ||
    (is.character(matching) &&
       length(matching) == 1L && !is.na(matching) &&
       toupper(matching) == "AUTO")
  if (!valid_matching) {
    fail("'matching' must be FALSE or 'AUTO'.")
  }

  if (!is_whole_number(maxiter) || maxiter < 1L) {
    fail("'maxiter' must be a positive integer.")
  }

  bad_max_models <- !is.null(max_models) &&
    (!is_whole_number(max_models) || max_models < 1L)
  if (bad_max_models) {
    fail(
      "'max_models' must be NULL or a positive integer."
    )
  }

  if (!is_scalar_number(tuning_time) ||
      tuning_time <= 0) {
    fail("'tuning_time' must be positive.")
  }
  if (!is_whole_number(cv) || cv < 5L) {
    fail("'cv' must be an integer of at least 5.")
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

  # Configure H2O memory
  min_ram <- max_ram <- NULL
  if (!is.null(ram)) {
    if (!is_whole_number(ram) || ram < 1L) {
      fail("'ram' must be a positive integer in GB.")
    }

    max_ram <- paste0(as.integer(ram), "G")
    total_ram_gb <- tryCatch(
      as.numeric(memuse::Sys.meminfo()$totalram) /
        1024^3,
      error = function(e) NA_real_
    )

    if (is.finite(total_ram_gb) && total_ram_gb > 0) {
      uses_xgb <- "XGBoost" %in% impute
      if (ram >= total_ram_gb) {
        warning(
          "H2O memory is at least the detected system RAM.",
          call. = FALSE
        )
      } else if (uses_xgb &&
                 ram > (2 / 3) * total_ram_gb) {
        warning(
          "XGBoost is enabled and H2O uses over 2/3 RAM.",
          call. = FALSE
        )
      } else if (ram > 0.80 * total_ram_gb) {
        warning(
          "H2O uses over 80% of detected system RAM.",
          call. = FALSE
        )
      }
    }
  }

  # Configure logging
  debug <- FALSE
  if (is.null(verbosity)) {
    verbose <- 0L
  } else {
    valid_verbosity <- is.character(verbosity) &&
      length(verbosity) == 1L && !is.na(verbosity) &&
      verbosity %in% c("warn", "info", "debug")
    if (!valid_verbosity) {
      fail(
        "'verbosity' must be NULL, warn, info, or debug."
      )
    }
    verbose <- switch(
      verbosity, warn = 1L, info = 2L, debug = 3L
    )
    debug <- identical(verbosity, "debug")
  }

  list(
    min_ram = min_ram, max_ram = max_ram,
    verbose = verbose, debug = debug
  )
}
