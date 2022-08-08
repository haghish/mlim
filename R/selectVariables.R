
#' @title select imputation variables
#' @description selects variables relevant to the imputation process
#' @importFrom md.log md.log
#' @param data data.frame
#' @return list
#' @keywords Internal
#' @noRd

selectVariables <- function(data, ignore=NULL, verbose=FALSE, md.log=NULL) {

  suppressPackageStartupMessages({requireNamespace("md.log")})

  # select the variables with missing, excluding fully missing vars
  vars2impute <- vapply(data[, , drop = FALSE], FUN.VALUE = TRUE,
                        function(z) anyNA(z) && !all(is.na(z)))

  # make sure that these variables were not meant to be ignored
  if (!is.null(ignore)) vars2impute[colnames(data)[vars2impute] %in% ignore] <- FALSE



  #data[, vars2impute] <- converted$X

  #if (verbose) {
  #  cat("\n  Variables to impute:\t\t")
  #  cat(colnames(data)[vars2impute], sep = ", ")
  #  cat("\n")
  #
  #}

  if (!is.null(md.log)) {
    md.log(paste("Variables to impute:", paste(colnames(data)[vars2impute],
                                               collapse = ", ")))
  }

  # Get missing indicators and order variables by number of missings
  dataNA <- is.na(data[, vars2impute, drop = FALSE])
  vars2impute <- names(sort(colSums(dataNA)))
  # ============================================================

  # specify the list of all predictors, which were not ignored by the user
  allPredictors <- colnames(data)[!colnames(data) %in% ignore]
  X <- setdiff(allPredictors, vars2impute)

  return(list(
    #data <- data,
    dataNA = dataNA,
    #converted = converted,
    allPredictors = allPredictors,
    vars2impute = vars2impute,
    X = X))
}



