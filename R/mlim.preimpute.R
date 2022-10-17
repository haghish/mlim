
#' @title carries out preimputation
#' @description instead of replacing missing data with mean and mode, a smarter
#'              start-point would be to use fast imputation algorithms and then
#'              optimize the imputed dataset with mlim. this procedure usually
#'              requires less iterations and will savea lot of computation
#'              resources.
# @importFrom VIM kNN
#' @importFrom missRanger missRanger imputeUnivariate
# @importFrom missForest missForest
#' @param data data.frame with missing values
#' @param preimpute character. specify the algorithm for preimputation. the
#'                  supported options are "RF" (Random Forest), "mm"
#'                  (mean-mode replacement), and "random" (random sampling from available data).
#'                  the default is "RF", which carries
#'                  a parallel random forest imputation, using all the CPUs available.
#'                  the other alternative is "mm" which performs mean/mode
#'                  imputation.
#' @param seed integer. specify the random generator seed
#' @return imputed data.frame
#' @author E. F. Haghish
#' @examples
#' \donttest{
#' data(iris)
#'
#' # add 10% stratified missing values to one factor variable
#' irisNA <- iris
#' irisNA$Species <- mlim.na(irisNA$Species, p = 0.1, stratify = TRUE, seed = 2022)
#'
#' # run the default random forest preimputation
#' MLIM <- mlim.preimpute(irisNA)
#' mlim.error(MLIM, irisNA, iris)

#' }
#' @export

mlim.preimpute <- function(data, preimpute = "RF", seed = NULL) {

  #if (tolower(preimpute) == "knn") {
  #  set.seed(seed)
  #  data <- VIM::kNN(data, imp_var=FALSE)
  #  if (!is.null(report)) md.log("kNN preimputation is done", date=debug, time=debug, trace=FALSE)
  #}

  if (tolower(preimpute) == "rf") {
    message("\nPreimputation: Random Forest")
    pb <- txtProgressBar(0, 1, style = 3)
    data <- missRanger::missRanger(data, num.trees=500, mtry=1,
                                   verbose = 0, returnOOB=TRUE, seed = seed)
    setTxtProgressBar(pb, 1)
    #if (!is.null(report)) md.log("RF preimputation is done", date=debug, time=debug, trace=FALSE)
  }
  else if (tolower(preimpute) == "mm") {
    message("\nPreimputation: Mean/Mode")
    pb <- txtProgressBar(0, 1, style = 3)
    data <- meanmode(data)
    setTxtProgressBar(pb, 1)
    #if (!is.null(report)) md.log("Mean/Mode preimputation is done", date=debug, time=debug, trace=FALSE)
  }
  else if (tolower(preimpute) == "sample") {
    message("\nPreimputation: Random Sampling")
    if (!is.null(seed)) set.seed(seed)
    rsample <- function(x) replace(x, is.na(x), sample(x[!is.na(x)],sum(is.na(x))))
    pb <- txtProgressBar(0, 1, style = 3)
    for (i in colnames(data)) {
      if (sum(is.na(data[,i])) > 0) data[,i] <- rsample(data[,i])
    }
    setTxtProgressBar(pb, 1)
  }
  else stop(paste(preimpute, "is not recognized preimputation argument"))

  return(data)
}



