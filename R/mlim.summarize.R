
#' @title mlim imputation summary
#' @description provides information about estimated accuracy of the imputation as well
#'              as the overall procedure of the imputation.
#' @param data dataset imputed with mlim
#' @return estimated imputation accuracy via cross-valdiation procedure
#' @author E. F. Haghish
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
#'
#' # compare the error with the estimated error from cross validation
#' mlim.summarize(MLIM)
#' }
#' @export
#' @export

mlim.summarize <- function(data) {

  results <- data.frame(variable = character(),
                        nrmse = numeric(),
                        mpce = numeric())

  if (inherits(data, "mlim")) {
    att <- attributes(data)
    metrics <- att$metrics
    VARS <- colnames(data)[colnames(data) %in% metrics$variable]
    for (i in VARS) {
      index <- metrics$variable == i
      if (inherits(data[,i], "factor")) {
        #err <- min(metrics[index, "mean_per_class_error"], na.rm = TRUE)
        minimum <- min(metrics[index, "RMSE"], na.rm = TRUE)
        err <- minimum / (length(levels(data[,i])) - 1)
        err <- round(err, 6)
        results <- rbind(results, c(i, err))
      }
      else {
        minimum <- min(metrics[index, "RMSE"], na.rm = TRUE)
        err <- minimum / diff(range(data[,i])) #sd(data[,i], na.rm = TRUE)
        err <- round(err, 6)
        results <- rbind(results, c(i, err))
      }
    }

    colnames(results) <- c("variable", "nrmse")
    return(results)
  }
  else if (inherits(data, "mlim.mi")) {
    results <- mlim.summarize(data[[1]])
    for (i in 2:length(data)) {
      results <- cbind(results, mlim.summarize(data[[i]])[,2])
    }
    colnames(results) <- c("varable", paste0("nrmse_", 1:length(data)))
    return(results)
  }

}

#print(mlim.summarize(default))



