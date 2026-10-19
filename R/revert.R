#' @title Revert ordinal variables to ordered factors
#' @description Converts numeric rank indices back to their original ordered
#'   factor levels using a factmem object. Finite values are rounded and
#'   constrained to the valid rank range to prevent out-of-range predictions
#'   from becoming missing values.
#' @param df A data.frame containing numeric rank indices.
#' @param factmem An ordinal-variable metadata object from factmem().
#' @return A data.frame with the recorded ordinal variables restored.
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

revert <- function(df, factmem) {
  cols <- names(df)

  for (i in seq_along(factmem)) {
    data <- factmem[[i]][[1]]
    if (is.null(data$names) || !data$names %in% cols) next

    x <- df[[data$names]]
    if (!is.numeric(x)) {
      stop("Expected numeric rank indices for ordinal variable '",
           data$names, "'.", call. = FALSE)
    }

    # Round to the nearest category and constrain predictions to valid ranks.
    # Original NA values remain NA; non-finite predictions are reported.
    invalid <- !is.na(x) & !is.finite(x)
    if (any(invalid)) {
      warning("Non-finite predictions in ordinal variable '", data$names,
              "' were converted to NA.", call. = FALSE)
      x[invalid] <- NA_real_
    }

    x <- round(x)
    x <- pmax(min(data$support), pmin(max(data$support), x))

    df[[data$names]] <- factor(
      as.character(x),
      levels = as.character(data$support),
      labels = data$level,
      ordered = TRUE
    )
  }

  df
}
