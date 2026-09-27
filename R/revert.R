#' @title revert
#' @description using factmem object, integer variables are
#'              reverted to the original variable type of
#'              "ordered factors".
#' @return data.frame
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

revert <- function(df, factmem) {
  cols <- colnames(df)

  for (i in seq_along(factmem)) {
    data <- factmem[[i]][[1]]

    if (!is.null(data$names)) {
      if (data$names %in% cols) {
        df[, data$names] <- factor(
          as.character(round(df[, data$names])),
          levels = as.character(data$support),
          labels = data$level,
          ordered = TRUE
        )
      }
    }
  }

  return(df)
}
