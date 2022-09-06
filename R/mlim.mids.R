#' @title prepare "mids" class object
#' @description takes "mlim" object and prepares a "mids" class for data analysis with
#'              multiple imputation.
#' @importFrom mice as.mids
#' @param mlim array of class "mlim", returned by "mlim" function
#' @param incomplete the original data.frame with NAs
#' @author E. F. Haghish, based on code from 'prelim' frunction in missMDA R package
#' @examples
#'
#' \donttest{
#' data(iris)
#' require(mice)
#' irisNA <- mlim.na(iris, p = 0.1, seed = 2022)
#'
#' # adding unstratified NAs to all variables of a data.frame
#' MLIM <- mlim(irisNA, m=5, tuning_time = 180, doublecheck = T, seed = 2022)
#'
#' # create the mids object for MICE package
#' mids <- mlim.mids(MLIM, irisNA)
#'
#' # run an analysis on the mids data (just as example)
#' fit <- with(data=mids, exp=glm(Species~ Sepal.Length, family = "binomial"))
#'
#' # then, pool the results!
#' summary(pool(fit))
#' }
#' @return object of class 'mids', as required by 'mice' package for analyzing
#'         multiple imputation data
#' @export

mlim.mids <- function (mlim, incomplete) {
    if (any(c("MIMCA", "MIPCA", "mlim.mi") %in% class(mlim))) {
      longformat <- rbind(incomplete, do.call(rbind, mlim))
      longformat <- cbind(.imp = rep(0:length(mlim), each = nrow(incomplete)),
                          .id = rep(1:nrow(incomplete), (length(mlim) + 1)), longformat)
      rownames(longformat) <- NULL
      mids <- as.mids(longformat)
    }
    else {
      stop("Objects of class mlim.mi, MIPCA, or MIMCA are required.")
    }

    return(mids)
}


#mid <- mlim.mids(ELNET, irisNA)
