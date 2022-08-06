
#' @title prepares an object of mlim class for postimputation
#' @description mlim is a computation intensive procedure. therefore it makes
#'              sense to secure the procedure by automating the process of
#'              saving the results of imputation. therefore, if R crashes, you
#'              need to follow the imputation from where it was stopped, instead
#'              of starting over. this process is carried out with "mlim.postimpute"
#'              function. however, "mlim.prepare" function only prepares the
#'              "mlim" class object needed for post imputation.
#' @param mlim.object object of class mlim to continue the
#' @param save imputed data.frame with missing values. for example, if you use
#'                "mlim.preimpute" function for preimputation, the returned data.frame
#'                can be handed to this argument.
#' @param algorithm character. specify the algorithm for preimputation. the
#'                  supported options are "kNN", "ranger", "missForest", and "mm"
#'                  (mean-mode replacement). "ranger" and "missForest" both
#'                  implement Random Forest procedures and are generally
#'                  recommended. For very large datasets use "kNN".
#' @return imputed data.frame
#' @author E. F. Haghish
#' @export

mlim.prepare <- function(data, imputed, md.log) {

  #mlimClass <- list(iteration=iterDF, k = k, include_algos=include_algos,
  #                  ignore=ignore, save = save, iterdata = iterdata,
  #                  maxiter = maxiter, miniter = miniter, nfolds = nfolds,
  #                  max_model_runtime_secs = max_model_runtime_secs,
  #                  max_models = max_models, matching = matching,
  #                  ordinal_as_integer = ordinal_as_integer,
  #                  weights_column = weights_column, seed=seed,
  #                  verbosity=verbosity, report=report,
  #                  iteration_stopping_metric=iteration_stopping_metric,
  #                  iteration_stopping_tolerance=iteration_stopping_tolerance,
  #                  stopping_metric=stopping_metric,stopping_rounds=stopping_rounds,
  #                  stopping_tolerance=stopping_tolerance,
  #                  nthreads = nthreads, max_mem_size=max_mem_size,
  #                  min_mem_size = min_mem_size,
  #
  #                  # save the package version used for the imputation
  #                  pkg=packageVersion("mlim")
  #                  )
  #class(mlimClass) <- "mlim"
}



