
#' @title generate multiple imputation datasets from a single imputation
#' @description mlim estimates Root Mean Squared Error (RMSE), which can provide
#'              a hint about the standard deviation of the error. if normal
#'              distribution of the error is assumed, stochastic imputed values
#'              can be generated, where each predicted value is considered the
#'              'mean' of the normal distribution and the standard deviation is
#'              assumed to be equal to 'RMSE'.
#' @param df original dataframe with NAs
#' @param imputed single imputed dataset of the class 'mlim'. the dataset must have
#'                been imuted with mlim R package to include the imputation metrics
#'                needed for the computation
#' @param m integer, specifying number of multiple imputations. the default value is
#'          1, carrying out a single imputation.
#' @param report filename. if a filename is specified (e.g. report = "mlim.md"), the \code{"md.log"} R
#'               package is used to generate a Markdown progress report for the
#'               imputation. the format of the report is adopted based on the
#'               \code{'verbosity'} argument. the higher the verbosity, the more
#'               technical the report becomes. if verbosity equals "debug", then
#'               a log file is generated, which includes time stamp and shows
#'               the function that has generated the message. otherwise, a
#'               reduced markdown-like report is generated. default is NULL.
#' @importFrom md.log md.log
#' @importFrom stats rnorm
#' @return list, including multiple imputed datasets
#' @author E. F. Haghish
#' @export
#'
mlim.shuffle <- function(df, imputed, m, report = NULL) {
  MI    <- NULL
  if (!is.null(report)) md.log("Stochastic Shuffling", section="paragraph", trace=FALSE)

  # get the imputation metrics
  metrics <- attributes(imputed)$metrics
  vars2impute <- unique(metrics$variable)
  dfNA <- is.na(df)

  for (i in 1:m) {
    MI[[i]] <- imputed

    for (Y in vars2impute) {
      v.na <- dfNA[, Y]
      RMSE <- min(metrics[metrics$variable == Y, "RMSE"], na.rm = TRUE)
      VEK <- imputed[which(v.na), Y]

      MI[[i]][which(v.na), Y] <- rnorm(
        n = length(VEK),
        mean = VEK,
        sd = RMSE)
    }
  }

  if (m > 1) class(MI) <- "mlim.mi"
  else class(MI) <- c("mlim", "data.frame")

  return(MI)
}


