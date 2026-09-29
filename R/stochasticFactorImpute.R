# ----------------------------------------------------------
# stochasticFactorImpute
# ==========================================================
#' @title Stochastic imputation of categorical variables
#' @description Samples one category for each missing observation
#'              according to its predicted class probabilities.
#' @param levels Character vector containing the factor levels.
#' @param probMat Matrix containing the predicted probability of
#'                each level for each missing observation.
#' @return A vector of sampled factor levels.
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

stochasticFactorImpute <- function(levels, probMat) {
  if (nrow(probMat) == 0L) return(character(0))

  vapply(seq_len(nrow(probMat)), function(i) {
      sample(x = levels, size = 1L, replace = TRUE, prob = probMat[i, ])
    },
    FUN.VALUE = character(1L)
  )
}
