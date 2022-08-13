#' @title iteration_loop
#' @description runs imputation iteration loop to fully impute a dataframe
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

iteration_loop <- function(dataNA, data, hex, metrics, tolerance, doublecheck,
                    k, X, Y, z,

                    # loop data
                    vars2impute, allPredictors, preimpute, impute, postimpute,

                    # settings
                    error_metric, FAMILY, cv, tuning_time,
                    max_models, weights_column,
                    keep_cross_validation_predictions,
                    balance, seed, save, flush,

                    verbose, debug, report, sleep,

                    # saving settings
                    dataLast, mem, orderedCols, ignore, maxiter,
                    miniter, matching, ignore.rank,
                    verbosity, error, cpu, max_ram, min_ram
                    ) {




  return(list(X=X,
              hex = hex,
              metrics = metrics,
              vars2impute=vars2impute))
}
