#' @title selects algorithms for imputation and post imputation
#' @description automatically sorts algorithms for imputation and post imputation
#' @author E. F. Haghish
#' @return integer
#' @keywords Internal
#' @noRd

algoSelector <- function(algos) {
  #preimpute <- NULL
  impute <- NULL
  postimpute <- NULL

  algos <- toupper(algos)
  if (length(setdiff(x=algos, y=c("ELNET","GLM",
                    "RF","DRF",
                    "GBM", "XGB","XGBoost",
                    "DL","DeepLearning",
                    "Ensemble","StackedEnsemble"))) > 0) stop("some of the 'algos' are not recognised")

  # impute (by default both GLM and DRF are used for imputing)
  # ------------------------------------------------------------
  if ("ELNET" %in% algos) impute <- c("GLM")
  else if ("GLM" %in% algos) impute <- c("GLM")

  if ("RF" %in% algos) impute <- c(impute, "DRF")
  else if ("DRF" %in% algos) impute <- c(impute, "DRF")

  # postimpute (start with GBM, then XGB, then DL, then Ensemble)
  # ------------------------------------------------------------
  if ("GBM" %in% algos) {
    if (is.null(impute)) impute <- "GBM"
    else postimpute <- c(postimpute, "GBM")
  }
  if ("XGB" %in% algos) {
    if (Sys.info()["sysname"] == "Windows") {
      stop("XGB is not available in Windows")
    }
    if (is.null(impute)) impute <- "XGBoost"
    else postimpute <- c(postimpute, "XGBoost")
  }
  if ("DL" %in% algos) {
    if (is.null(impute)) impute <- "DeepLearning"
    else postimpute <- c(postimpute, "DeepLearning")
  }

  # if Ensemble is specified, include the 'impute' algorithms
  if ("Ensemble" %in% algos) {
    if (is.null(impute)) stop("Ensemble is a meta learner and requires other algorithms. ")
    else postimpute <- c(impute, postimpute, "StackedEnsemble")
  }

  return(list(impute=impute, postimpute=postimpute))
}


