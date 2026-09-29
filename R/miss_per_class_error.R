#' @title Mean Per Class Error (MPCE)
#' @description calculates the missclassification rate for each level of an unordered
#'              factor and either returns a vector of the errors or return the mean,
#'              if 'mean = TRUE'
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @param mean logical. if TRUE (defualt), the mean per class error is returned
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
mean_per_class_error <- function(imputed, incomplete, complete, mean = FALSE){
  mpce <- NULL
  for (k in 1:ncol(complete)) {
    lvl <- levels(complete[, k])
    for (i in lvl) {
      index <- drop(complete[, k]) == i
      temp <- missclass(imputed[index, k, drop = FALSE],
                               incomplete[index, k, drop = FALSE],
                               complete[index, k, drop = FALSE],
                               rename = FALSE)
      names(temp) <- i
      if (!is.null(temp)) mpce <- c(mpce, temp)
    }
  }

  if (mean) mpce <- mean(mpce)
  return(mpce)
}

# print(mean_per_class_error(mm[,c("education"), drop = FALSE],
#                                   dfNA[,c("education"), drop = FALSE],
#                                   df[,c("education"), drop = FALSE]))

# index <- df$education == "HS" & is.na(dfNA$education)
# table(index)
# mm$education[index]
#index <- is.na(dfNA$education[index])
#table(index)
#View(cbind(df$education[index], mm$education[index]))
