#' @title normalized RMSE
#' @description calculates the normalized RMSE
#' @importFrom curl curl
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
#'
.onAttach <- function(lib, pkg) {
  packageStartupMessage('Multiple Imputation with Automated Machine Learning\nhttps://github.com/haghish/mlim\n\nNote1: The package receives frequent updates. Installing \n       the development version from GitHub is recommended. \n       A free handbook is also available for download at the \n       GitHub repository.\nNote2: If you find a bug, post it on GitHub or e-mail it to \n       haghish@uio.no\n')
  options("prefer_RCurl" = FALSE)
  options(timeout = 3600) # "seconds"
}
