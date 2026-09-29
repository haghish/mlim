# ----------------------------------------------------------
# getMetrics
# ==========================================================
#' @title generates random integers with the length of the factor levels
#' @description generates stochastic integer valuesbased on estimated
#'              probabilities of each factor's levels' probabilities
#' @param levels levels of the factor
#' @param probMat estimated probability of each level for each missing observation
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

stochasticFactorImpute <- function(levels, probMat)
{
  do.call("c", lapply(seq(nrow(probMat)), function(i)
  {
    sample(x = levels, size = 1, replace = TRUE, prob = probMat[i,])
  }))
}

