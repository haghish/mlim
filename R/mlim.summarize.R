
#' @title mlim imputation summary
#' @description provides information about estimated accuracy of the imputation as well
#'              as the overall procedure of the imputation.
#' @param data dataset imputed with mlim
# @param method character. the default is NULL, returning RMSE. the supported
#               arguments are "scaled" nad "normalize".
#' @return estimated imputation accuracy via cross-valdiation procedure
#' @examples
#' \dontrun{
#' data(iris)
#'
#' # add 10% stratified missing values to one factor variable
#' irisNA <- iris
#' irisNA$Species <- mlim.na(irisNA$Species, p = 0.1, stratify = TRUE, seed = 2022)
#'
#' # run the ELNET single imputation (fastest imputation via 'mlim')
#' MLIM <- mlim(irisNA)
#'
#' # in single imputation, you can estimate the imputation accuracy via cross validation RMSE
#' mlim.summarize(MLIM)
#' }
#' @author E. F. Haghish
#' @export

mlim.summarize <- function(data #, method = NULL
                           ) {

  results <- data.frame(variable = character(),
                        nrmse = numeric(),
                        mpce = numeric())

  if (inherits(data, "mlim")) {
    att <- attributes(data)
    metrics <- att$metrics
    VARS <- colnames(data)[colnames(data) %in% metrics$variable]
    for (i in VARS) {
      err <- NULL
      index <- metrics$variable == i
      if (inherits(data[,i], "factor")) {
        #err <- min(metrics[index, "mean_per_class_error"], na.rm = TRUE)
        minimum <- min(metrics[index, "RMSE"], na.rm = TRUE)
        # if (is.valid(method)) {
        #   if (method == "scale") err <- minimum / (length(levels(data[,i])) - 1)
        # }
        err <- round(minimum, 6)
        results <- rbind(results, c(i, err))
      }
      else {
        minimum <- min(metrics[index, "RMSE"], na.rm = TRUE)
        # if (is.valid(method)) {
        #   if (method == "scale") err <- minimum / diff(range(data[,i])) #sd(data[,i], na.rm = TRUE)
        #   else if (method == "scale") err <- minimum / sd(data[,i], na.rm = TRUE)
        # }

        err <- round(minimum, 6)
        results <- rbind(results, c(i, err))
      }
    }

    colnames(results) <- c("variable", "rmse")
    return(results)
  }
  else if (inherits(data, "mlim.mi")) {
    results <- mlim.summarize(data[[1]])
    for (i in 2:length(data)) {
      results <- cbind(results, mlim.summarize(data[[i]])[,2])
    }
    colnames(results) <- c("varable", paste0("rmse_", 1:length(data)))
    return(results)
  }

}

#print(mlim.summarize(default))



