
#' @title extractMetrics
#' @description extracts performance metrics from cross-validation
#' @return data.frame of error metrics.
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

extractMetrics <- function(hex, k, v, perf, family) {

  # calculate the variance of v
  if (is.numeric(var)) {
    var <- var(as.vector(hex[, v]), na.rm = TRUE)
    NRMSE <- as.numeric(perf@metrics$RMSE)/var
  } else {
    NRMSE <- NA
  }

  RMSE = as.numeric(perf@metrics$RMSE)
  MSE = as.numeric(perf@metrics$MSE)
  MAE = as.numeric(perf@metrics$mae)
  if (length(MAE) < 1) MAE <- NA
  RMSLE = as.numeric(perf@metrics$rmsle)
  if (length(RMSLE) < 1) RMSLE <- NA
  Mean_Residual_Deviance = as.numeric(perf@metrics$mean_residual_deviance)
  if (length(Mean_Residual_Deviance) < 1) Mean_Residual_Deviance <- NA
  R2 = as.numeric(perf@metrics$r2)
  #AIC = numeric()
  logloss = as.numeric(perf@metrics$logloss)
  if (length(logloss) < 1) logloss <- NA
  mean_per_class_error = as.numeric(perf@metrics$mean_per_class_error)
  if (length(mean_per_class_error) < 1) mean_per_class_error <- NA
  AUC = as.numeric(perf@metrics$AUC)
  if (length(AUC) < 1) AUC <- NA
  pr_auc = as.numeric(perf@metrics$pr_auc)
  if (length(pr_auc) < 1) pr_auc <- NA

  metrics <- data.frame(iteration=k,
                        variable = v,
                        NRMSE = NRMSE,
                        RMSE = RMSE,
                        MSE = MSE,
                        MAE = MAE,
                        RMSLE = RMSLE,
                        Mean_Residual_Deviance = Mean_Residual_Deviance,
                        R2 = R2,
                        #AIC = numeric()
                        logloss = logloss,
                        mean_per_class_error = mean_per_class_error,
                        AUC = AUC,
                        pr_auc = pr_auc)

  return(metrics)
}
