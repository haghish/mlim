#' @title revert
#' @description using factmem object, integer variables are reverted to the
#'              original variable type of "ordered factors".
#' @return data.frame
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

revert <- function(df, factmem) {
  cols <- colnames(df)
  for (i in 1:length(factmem)) {
    data <- factmem[[i]][[1]]
    if (!is.null(data$names)) {
      if (data$names %in% cols) {
        df[, data$names] <- factor(as.character(round(df[, data$names])),
                                   levels = as.character(data$length),
                                   labels = data$level,
                                   ordered = TRUE)
      }

    }
  }

  return(df)
}

#revert(round(mlimELNET[, 1:3]), MEM)
#print(head(mlimELNET$A1))

#revert(LAST[, COLS, drop = FALSE], MEM)

