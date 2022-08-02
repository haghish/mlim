
#' @title is.imbalanced
#' @description examines whether a binary or multinomial feature is imbalanced
#' @param x vector.
#' @author E. F. Haghish
#' @return data.frame of error metrics.
#' @export

is.imbalanced <- function(x) {
  imbalanced <- FALSE
  prev <- sort(table(x) / length(x))

  # get the difference between the first and last scalars
  diff <- prev[1] / prev[length(prev)]

  # define a criteria for imbalance, to prefer AUCPR to AUC.
  # Here I select any difference more than 0.3 (e.g. 70% - 30%) as
  # imbalanced and prefer AUPRC for tuning
  if (diff >= 0.3) imbalanced <- TRUE

  return(imbalanced)
}



