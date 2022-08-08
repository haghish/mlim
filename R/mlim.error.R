#' @title normalized RMSE
#' @description calculates the normalized RMSE
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
nrmse <- function(imputed, incomplete, complete){
  nrmse <- NULL
  mis <- as.data.frame(is.na(incomplete))
  index <- which(colSums(mis) > 0)
  for (i in index) {
    v.na <- is.na(incomplete[, i])
    nrmse <- c(nrmse,
               sqrt(mean((imputed[v.na,i] - complete[v.na,i])^{2}) /
                      stats::var(complete[v.na,i]))
               )

  }
  names(nrmse) <- colnames(incomplete)[index]
  return(nrmse)
  #sqrt(mean((imputed[mis] - complete[mis])^{2}) / stats::var(complete[mis]))
}

#' @title miss ranking error
#' @description calculates the MAE or missranking, devided by N-1 ordered categories
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
missrank <- function(imputed, incomplete, complete){
  MAE <- NULL
  mis <- as.data.frame(is.na(incomplete))
  index <- which(colSums(mis) > 0)
  for (i in index) {
    imputed[,i] <- as.numeric(imputed[,i])
    incomplete[,i] <- as.numeric(incomplete[,i])
    complete[,i] <- as.numeric(complete[,i])
    v.na <- is.na(incomplete[, i])
    MAE <- c(MAE, (mean(abs(imputed[v.na,i] - complete[v.na,i])) /
                     (length(unique(complete[,i]))-1)
                   ))
  }
  names(MAE) <- colnames(incomplete)[index]
  return(MAE)
}




#' @title missclassification error
#' @description calculates the missclassification rate for each variable
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
missclass <- function(imputed, incomplete, complete){
  classerror <- NULL
  mis <- as.data.frame(is.na(incomplete))
  index <- which(colSums(mis) > 0)
  for (i in index) {
    missclass <- sum(as.character(as.matrix(imputed[,i])) != as.character(as.matrix(complete[,i])))
    NAs <- sum(is.na(incomplete[, i]))
    classerror <- c(classerror, (missclass / NAs))
  }
  names(classerror) <- colnames(incomplete)[index]
  return(classerror)
}


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
  else return(err)

}

