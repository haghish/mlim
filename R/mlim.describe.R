
#' @title mlim imputation summary
#' @description provides information about estimated accuracy of the imputation as well
#'              as the overall procedure of the imputation.
#' @importFrom missRanger missRanger imputeUnivariate
# @importFrom missForest missForest
#' @param data data.frame with missing values
#' @param preimpute character. specify the algorithm for preimputation. the
#'                  supported options are "RF" (Random Forest) and "mm"
#'                  (mean-mode replacement). the default is "RF", which carries
#'                  a parallel random forest imputation, using all the CPUs available.
#' @param report filename. if a filename is specified, the \code{"md.log"} R
#'               package is used to generate a Markdown progress report for the
#'               imputation. the format of the report is adopted based on the
#'               \code{'verbosity'} argument. the higher the verbosity, the more
#'               technical the report becomes. if verbosity equals "debug", then
#'               a log file is generated, which includes time stamp and shows
#'               the function that has generated the message. otherwise, a
#'               reduced markdown-like report is generated.
#'               this feature is used internally by 'mlim' function.
#' @param debug. logical. if TRUE, the debugging feature of the report is activated.
#'               this feature is used internally by 'mlim' function.
#' @return imputed data.frame
#' @author E. F. Haghish
#' @export

mlim.summarize <- function(data) {

  att <- attributes(data)

  return(data)
}



