#' @title normalized RMSE
#' @description calculates the normalized RMSE
#' @importFrom curl curl
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
#'
.onAttach <- function(lib, pkg) {
  packageStartupMessage('Single and Multiple Imputation with Automated Machine Learning\n
                      \nNote1: This is a free software and comes with no guarantee.
                      \nNote2: The default algorithm is ELNET, which is the fastest\n       to fine-tune. You can reduce imputation error by\n       adding other algorithms e.g. "RF", "Ensemble", ...
                      \nNote3: If you find a bug, post it on GitHub\n       https://github.com/haghish/mlim\n')
  options("prefer_RCurl" = FALSE)
  options(timeout = 3600) # "seconds"
}
