#' NOTE:
#' ====
#'
#' This file includes "RF" for randomforest preimputation. Such an approach can
#' inflate relationships between the features and thus, this method is not documented
#' and is not recommended. It is maintained in the code for research purpose only.

#' @title Preimputation of missing values
#' @description
#' Initializes missing values before the iterative mlim imputation procedure.
#' Missing values can be initialized using median/mode, random sampling,
#' from the observed values of each variable, or Random Forest imputation (experimental).
#' @param data data.frame containing missing values
#' @param preimpute character. Specify the algorithm for preimputation.
#'                  Supported options are "mm" (median/mode replacement),
#'                  "random" for random sampling from available data.
#' @param seed integer. Random-number seed used by Random Forest or random
#'   sampling. The default is \code{NULL}.
#'
#' @importFrom missRanger missRanger imputeUnivariate
#' @return imputed data.frame
#' @author E. F. Haghish
#' @examples
#' \dontrun{
#' data(iris)
#'
#' # add 10% stratified missing values to one factor variable
#' irisNA <- iris
#' irisNA$Species <- mlim.na(irisNA$Species, p = 0.1, stratify = TRUE, seed = 2022)
#'
#' # run the default Median/Model preimputation
#' MLIM <- mlim.preimpute(irisNA, preimpute = "mm")
#' mlim.error(MLIM, irisNA, iris) #check the preimputation error
#'
#' # Random-sampling preimputation
#' RANDOM <- mlim.preimpute(irisNA, preimpute = "random", seed = 2022)
#' }
#' @export

mlim.preimpute <- function(data, preimpute = "mm", seed = NULL) {

  # Syntax check
  # ============================================================
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!is.character(preimpute) || length(preimpute) != 1L) stop("'preimpute' must be a single character value.")

  preimpute <- tolower(preimpute)
  if (!preimpute %in% c("rf", "mm", "random")) stop("'preimpute' must be one of 'RF', 'mm', or 'random'.")

  #if (tolower(preimpute) == "knn") {
  #  set.seed(seed)
  #  data <- VIM::kNN(data, imp_var=FALSE)
  #  if (!is.null(report)) md.log("kNN preimputation is done", date=debug, time=debug, trace=FALSE)
  #}

  # Note that RF is experimental and is not yet documented (on purpose).
  if (preimpute == "rf") {
    message("\nPreimputation: Random Forest")
    pb <- txtProgressBar(0, 1, style = 3)
    data <- missRanger::missRanger(data, num.trees=500, mtry=1,
                                   verbose = 0, returnOOB=TRUE, seed = seed)
    setTxtProgressBar(pb, 1)
    #if (!is.null(report)) md.log("RF preimputation is done", date=debug, time=debug, trace=FALSE)
  }
  else if (preimpute == "mm") {
    message("\nPreimputation: Median/Mode")
    pb <- txtProgressBar(0, 1, style = 3)
    data <- medianmode(data)
    setTxtProgressBar(pb, 1)
    #if (!is.null(report)) md.log("Median/Mode preimputation is done", date=debug, time=debug, trace=FALSE)
  }
  else if (preimpute == "random") {
    message("\nPreimputation: Random Sampling")
    if (!is.null(seed)) set.seed(seed)
    rsample <- function(x) replace(x, is.na(x), sample(x[!is.na(x)],sum(is.na(x)), replace = TRUE))
    pb <- txtProgressBar(0, 1, style = 3)
    for (i in colnames(data)) {
      if (sum(is.na(data[,i])) > 0) data[,i] <- rsample(data[,i])
    }
    setTxtProgressBar(pb, 1)
  }

  return(data)
}



