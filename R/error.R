#' @title normalized RMSE
#'
#' @description calculates the normalized RMSE
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
nrmse <- function(imputed, incomplete, complete){
  mis <- is.na(incomplete)
  sqrt(mean((imputed[mis] - complete[mis])^{2}) / stats::var(complete[mis]))
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
  mis <- is.na(incomplete)
  index <- which(colSums(mis) > 0)
  MAE <- NULL
  for (i in index) {
    imputed[,i] <- as.numeric(imputed[,i])
    incomplete[,i] <- as.numeric(incomplete[,i])
    complete[,i] <- as.numeric(complete[,i])
    v.na <- is.na(incomplete[, i])
    MAE <- c(MAE, (mean(abs(imputed[v.na,i] - complete[v.na,i])) /
                     (length(unique(complete[,i]))-1)
                   ))
  }
  return(MAE)
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


mlim.error <- function(imputed, incomplete, complete) {
  classes <- lapply(complete, class)
  types <- character(length(classes))
  for (i in 1:length(classes)) types[i] <- classes[[i]][1]

  n <- nrow(imputed)
  err <- rep(NA, 3)
  names(err) <- c('nrmse', 'missclass', 'missrank')

  for (t in types){
    ind <- which(types == t)

    if (t == "numeric") {
      err[1] <- nrmse(imputed[,ind], incomplete[,ind], complete[,ind])
    }
    else if (t == 'ordered') {
      rankerror <- missrank(imputed[,ind],
                     incomplete[,ind],
                     complete[,ind])
      err[3] <- mean(rankerror, na.rm = TRUE)
    }
    else {
      dist <- sum(as.character(as.matrix(imputed[,ind])) != as.character(as.matrix(complete[,ind])))
      no.na <- sum(is.na(incomplete[,types == 'factor']))
      if (no.na == 0){
        err[2] <- 0
      } else {
        err[2] <- dist / no.na
      }
    }
  }
  return(err)
}


