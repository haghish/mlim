#' @title prepare "mids" class object
#' @description takes "mlim" object and prepares a "mids" class for data analysis with
#'              multiple imputation.
#' @param mlim array of class "mlim", returned by "mlim" function
#' @param X
#' @param seed integer. a random seed number for reproducing the result (recommended)
#' @author E. F. Haghish
#' @examples
#'
#' \donttest{
#' # adding stratified NA to an atomic vector
#' x <- as.factor(c(rep("M", 100), rep("F", 900)))
#' table(mlim.na(x, p=.5, stratify = TRUE))
#'
#' # adding unstratified NAs to all variables of a data.frame
#' data(iris)
#' mlim.na(iris, p=0.5, stratify = FALSE, seed = 1)
#'
#' # or add stratified NAs only to factor variables, ignoring other variables
#' mlim.na(iris, p=0.5, stratify = TRUE, classes = "factor", seed = 1)
#'
#' # or add NAs to numeric variables
#' mlim.na(iris, p=0.5, classes = "numeric", seed = 1)
#' }
#' @export

mlim.mids <- function (mlim, X) {
    if (any(c("MIFAMD", "MIPCA","MIMCA") %in% class(mlim))) {
      longformat <- rbind(X, do.call(rbind, mlim$res.MI))
      longformat <- cbind(.imp = rep(0:length(mlim$res.MI),
                                     each = nrow(X)),
                          .id = rep(1:nrow(X), (length(mlim$res.MI) + 1)), longformat)
      rownames(longformat) <- NULL
      mids <- as.mids(longformat)
    }
    else {
      stop("prelim requires as input an object of class MIPCA or MIMCA.")
    }

    return(mids)
}