#' @title revert
#' @description using factmem object, integer variables are reverted to the
#'              original variable type of "ordered factors".
#' @return data.frame
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

revert <- function(df, factmem) {
  for (i in 1:length(factmem)) {
    data <- factmem[[i]]
    print(data)

    df[,data$names] <- factor(df[,data$names],
                              levels = data$length,
                              labels = data$level,
                              ordered = TRUE)
  }

  return(df)
}
