
#' @title carries out postimputation to minimize imputation error
#' @description this is a function for carrying out postimputation on datasets
#'              that are already imputed, either by mlim or other software.
#' @param data data.frame with missing values
#' @param preimputed.data data.frame. if you have used another software for missing
#'                      data imputation, you can still optimize the imputation
#'                      by handing the data.frame to this argument, which will
#'                      bypass the "preimpute" procedure.
#' @param algos character vector of algorithms to be used for imputation. the default
#'              is 'c("ELNET", "GBM", "RF", "DL", "Ensemble")'.
#' @param ... arguments to be passed to mlim
#' @return imputed data.frame
#' \donttest{
#' data(iris)
#'
#' # add 10% stratified missing values to one factor variable
#' irisNA <- iris
#' irisNA$Species <- mlim.na(irisNA$Species, p = 0.1, stratify = TRUE, seed = 2022)
#'
#' # run the default imputation
#' MLIM <- mlim(irisNA)
#' mlim.error(MLIM, irisNA, iris)
#'
#' # carry out postimputation. for real-life applications, you should increase
#' # the 'tuning_time' to more than one 3600 seconds, even for small datasets
#' post <- mlim.postimpute(data=irisNA, preimputed.data=MLIM, tuning_time=120,
#'         algos = c("GBM", "RF", "ELNET", "Ensemble"),
#'         seed = 2022)
#' }
#' @author E. F. Haghish
#' @export

mlim.postimpute <- function(data, preimputed.data,
                            algos = c("ELNET", "GBM", "RF", "DL", "Ensemble"),
                            ... ) {
  results <- NULL

    if (inherits(preimputed.data, "mlim.mi")) {
      results <- list()
      for (i in 1:length(preimputed.data)) {
        results[[i]] <- mlim(data = data,
                             preimputed.data = preimputed.data[[i]],
                             algos = algos,
                             postimpute = FALSE,
                             ...)
      }
    }

    # if the preimputation was done with mlim, extract the metrics
    else if (inherits(preimputed.data, "mlim")) {
      results <- mlim(data = data,
                      preimputed.data = preimputed.data,
                      algos = algos,
                      postimpute = FALSE,
                      ...)
    }

  return(results)
}



