#' @title imputation error
#' @description calculates NRMSE, missclassification rate, and miss-ranking
#'              absolute mean distance, scaled from 0 to 1, where 1 means
#'              maximum distance between the actual rank of a level with the
#'              imputed level.
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @author E. F. Haghish
#' @export
mlim.error <- function(imputed, incomplete, complete,
                       varwise = FALSE, ignore.rank=FALSE) {
  rankerror <- NA
  err <- NULL

  # make sure the complete dataset is complete!
  if (length(which(colSums(is.na(df)) > 0)) > 0)
    stop("'complete dataset has missing values")

  # get the variables with missing data, ignoring the rest
  naCols <- which(colSums(is.na(incomplete)) > 0)
  imputed <- imputed[, naCols, drop = FALSE]
  incomplete <- incomplete[, naCols, drop = FALSE]
  complete <- complete[, naCols, drop = FALSE]


  classes <- lapply(complete, class)
  types <- character(length(classes))
  for (i in 1:length(classes)) types[i] <- classes[[i]][1]
  if ("integer" %in% types) types[which(types == "integer")] <- "numeric"

  n <- nrow(imputed)

  if (!ignore.rank) {
    err <- rep(NA, 3)
    names(err) <- c('nrmse', 'missclass', 'missrank')
  }
  else {
    err <- rep(NA, 2)
    names(err) <- c('nrmse', 'missclass')
    if ("ordered" %in% types) types[which(types == "ordered")] <- "factor"
  }

  for (t in types){
    ind <- which(types == t)

    if (t == "numeric") {
      nrmse <-  nrmse(imputed[,ind, drop = FALSE],
                      incomplete[,ind, drop = FALSE],
                      complete[,ind, drop = FALSE])
      if (!is.null(nrmse)) err[1] <- mean(nrmse, na.rm = TRUE)
    }
    else if (t == 'ordered' & !ignore.rank) {
      rankerror <- missrank(imputed[,ind, drop = FALSE],
                     incomplete[,ind, drop = FALSE],
                     complete[,ind, drop = FALSE])
      if (!is.null(rankerror))  err[3] <- mean(rankerror, na.rm = TRUE)
    }
    else {
      classerror <- missclass(imputed[,ind, drop = FALSE],
                              incomplete[,ind, drop = FALSE],
                              complete[,ind, drop = FALSE])
      if (!is.null(classerror)) err[2] <- mean(classerror, na.rm = TRUE)
    }
  }


  if (varwise) return(list(mean = err,
                           nrmse = nrmse,
                           missclass = classerror,
                           missrank = rankerror))
  else return(err[!is.na(err)])

}

#print((ELNETerror <- mlim.error(ELNET, dfNA, df)))