
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
#' @author E. F. Haghish
#' @export

mlim.postimpute <- function(data, preimputed.data,
                            algos = c("ELNET", "GBM", "RF", "DL", "Ensemble"),
                            ... ) {

    if (inherits(preimputed.data, "mlim.mi")) {
      results <- list
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



