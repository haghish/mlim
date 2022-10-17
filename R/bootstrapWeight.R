#' @title bootstrap duplication removed and weight_column calculator
#' @description identifies duplicated rows in the bootstrap and creates
#'              a weight column counting the duplicated rows to be given
#'              as additional weights. this weight column should be
#'              added to the weight_column given by the user.
#' @author E. F. Haghish
#' @return numeric data.frame including 'rows' and 'weight'. rows is the row number
#'         of the observation and weight is the number of duplications.
#' @keywords Internal
#' @noRd
bootstrapWeight <- function(index) {
  weight <- table(index)
  rows <- as.numeric(names(weight))
  return(as.data.frame(cbind(rows, weight)))
}
