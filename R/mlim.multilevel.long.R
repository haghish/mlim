#' @title Restore Multilevel Data to Long Format
#' @description Restores long data that were automatically reshaped to wide
#'   format by `mlim.multilevel()`.
#' @param data A wide data frame returned by `mlim.multilevel()`.
#' @param original_only If `TRUE` (default), returns only variables present in
#'   the original input; otherwise generated multilevel summaries are restored too.
#' @return A data frame in the original long row order. If the original input was
#'   already wide, the data are returned without reshaping.
#' @author E. F. Haghish
#' @keywords internal
#' @noRd
mlim.multilevel.long <- function(data, original_only = TRUE) {

  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!identical(attr(data, "mlim.multilevel.format"), "wide")) {
    stop("'data' was not created by mlim.multilevel().")
  }

  hierarchy <- attr(data, "mlim.hierarchy")
  original_variables <- attr(data, "mlim.original.variables")
  input_format <- attr(data, "mlim.multilevel.input.format")
  if (is.null(hierarchy) || is.null(original_variables) || is.null(input_format)) {
    stop("The multilevel metadata are incomplete and the original data cannot be restored.")
  }

  if (input_format == "wide") {
    if (isTRUE(original_only)) {
      missing_variables <- setdiff(original_variables, names(data))
      if (length(missing_variables)) {
        stop("Original variables are missing from the data: ", paste(missing_variables, collapse = ", "))
      }
      data <- data[, original_variables, drop = FALSE]
    }
    return(data)
  }

  long_columns <- attr(data, "mlim.wide.long.columns")
  mapping <- attr(data, "mlim.wide.mapping")
  times <- attr(data, "mlim.wide.times")
  column_map <- attr(data, "mlim.wide.column.map")
  metadata <- list(long_columns, mapping, times, column_map)
  if (any(vapply(metadata, is.null, logical(1)))) {
    stop("The wide multilevel metadata are incomplete and the original long representation cannot be reconstructed.")
  }
  if (!all(hierarchy %in% names(data))) {
    stop("The hierarchy variables required to restore the long data are missing.")
  }

  make_unit_key <- function(df) do.call(paste, c(lapply(df, as.character), list(sep = "\u001f")))
  row_index <- match(mapping$.mlim_unit_key, make_unit_key(data[hierarchy]))
  if (anyNA(row_index)) {
    stop("Some hierarchy units in the stored long-data mapping are not present in the wide data.")
  }

  variables <- if (isTRUE(original_only)) original_variables else long_columns
  result <- setNames(vector("list", length(variables)), variables)

  for (variable in variables) {
    if (variable %in% hierarchy) {
      result[[variable]] <- mapping[[variable]]
      next
    }

    wide_names <- column_map[[variable]]
    if (is.null(wide_names)) stop("No wide-column mapping was stored for variable '", variable, "'.")
    if (!all(wide_names %in% names(data))) {
      stop("Wide columns required to restore variable '", variable, "' are missing: ",
           paste(setdiff(wide_names, names(data)), collapse = ", "))
    }

    restored <- data[[wide_names[1L]]][rep(NA_integer_, nrow(mapping))]
    for (j in seq_along(times)) {
      rows <- which(mapping$.mlim_occasion == times[j])
      if (length(rows)) restored[rows] <- data[[wide_names[j]]][row_index[rows]]
    }
    result[[variable]] <- restored
  }

  result <- as.data.frame(result, check.names = FALSE, stringsAsFactors = FALSE)
  result <- result[order(mapping$.mlim_row_id), , drop = FALSE]
  rownames(result) <- NULL

  if (!isTRUE(original_only)) {
    attr(result, "mlim.hierarchy") <- hierarchy
    attr(result, "mlim.original.variables") <- original_variables
    attr(result, "mlim.multilevel.variables") <- setdiff(names(result), original_variables)
    attr(result, "mlim.multilevel.format") <- "long"
    attr(result, "mlim.multilevel.input.format") <- "long"
  }
  result
}
