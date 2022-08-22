#' @title imputation error
#' @description calculates NRMSE, missclassification rate, and miss-ranking
#'              absolute mean distance, scaled between 0 to 1, where 1 means
#'              maximum distance between the actual rank of a level and the
#'              imputed level.
#' @param imputed the imputed dataframe
#' @param incomplete the dataframe with missing values
#' @param complete the original dataframe with no missing values
#' @param transform character. it can be either "standardize", which standardizes the
#'                numeric variables before evaluating the imputation error, or
#'                "normalize", which change the scale of continuous variables to
#'                range from 0 to 1. the default is NULL.
#' @param varwise logical, default is FALSE. if TRUE, in addition to
#'                mean accuracy for each variable type, the algorithm's
#'                performance for each variable (column) of the datast is
#'                also returned. if TRUE, instead of a numeric vector, a
#'                list is retuned.
#' @param ignore.missclass logical. the default is TRUE. if FALSE, the overall
#'                missclassification rate for imputed unordered factors will be
#'                returned. in general, missclassification is not recommended,
#'                particularly for multinomial factors because it is not robust
#'                to imbalanced data. in other words, an imputation might show
#'                a very high accuracy, because it is biased towards the majority
#'                class, ignoring the minority levels. to avoid this error,
#'                Mean Per Class Error (MPCE) is returned, which is the average
#'                missclassification of each class and thus, it is a fairer
#'                criteria for evaluating multinomial classes.
#' @param ignore.rank logical (default is FALSE, which is recommended). if TRUE,
#'                the accuracy of imputation of ordered factors (ordinal variables)
#'                will be evaluated based on 'missclassification rate' instead of
#'                normalized euclidean distance. this practice is not recommended
#'                because higher classification rate for ordinal variables does not
#'                guarantee lower distances between the imputed levels, despite the
#'                popularity of evaluating ordinal variables based on missclassification
#'                rate. in other words, assume an ordinal variable has 5 levels (1. strongly
#'                disagree, 2. disagree, 3. uncertain, 4. agree, 5.strongly agree). in this
#'                example, if "ignore.rank = TRUE", then an imputation that imputes level
#'                "5" as "4" is equally inaccurate as other algorithm that imputes level "5"
#'                as "1". therefore, if you have ordinal variables in your dataset, make sure
#'                you declare them as "ordered" factors to get the best imputation accuracy.

#' @author E. F. Haghish
#' @examples
#'
#' \donttest{
#' data(iris)
#'
#' # add 10% missing values, ensure missingness is stratified for factors
#' irisNA <- mlim.na(iris, p = 0.1, stratify = TRUE, seed = 2022)
#'
#' # run the default imputation
#' MLIM <- mlim(irisNA)
#' mlim.error(MLIM, irisNA, iris)
#'
#' # get error estimations for each variable
#' mlim.error(MLIM, irisNA, iris, varwise = TRUE)
#' }
#' @return numeric vector
#' @export
mlim.error <- function(imputed, incomplete, complete, transform = NULL,
                       varwise = FALSE, ignore.missclass = TRUE,
                       ignore.rank=FALSE) {

  rankerror    <- NA
  classerror   <- NA
  meanclasserr <- NA
  nrmse        <- NA
  err          <- NULL

  if ("mlim" %in% class(imputed) | "data.frame" %in% class(imputed) ) {
    # make sure the complete dataset is complete!
    if (length(which(colSums(is.na(complete)) > 0)) > 0)
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
      err <- rep(NA, 4)
      names(err) <- c('nrmse', 'mpce', 'missclass', 'missrank')
    }
    else {
      err <- rep(NA, 3)
      names(err) <- c('nrmse', 'mpce', 'missclass')
      if ("ordered" %in% types) types[which(types == "ordered")] <- "factor"
    }

    for (t in types){
      ind <- which(types == t)

      if (t == "numeric") {
        if (is.null(transform)) {
          nrmse <-  nrmse(imputed[,ind, drop = FALSE],
                          incomplete[,ind, drop = FALSE],
                          complete[,ind, drop = FALSE])
          if (!is.null(nrmse)) err[1] <- mean(nrmse, na.rm = TRUE)
        }
        else {
          if (transform == "standardize") {
            v1 <- scale(imputed[,ind, drop = FALSE])
            v2 <- scale(incomplete[,ind, drop = FALSE])
            v3 <- scale(complete[,ind, drop = FALSE])
          }
          else if (transform == "normalize") {
            v1 <- normalize(imputed[,ind, drop = FALSE])
            v2 <- normalize(incomplete[,ind, drop = FALSE])
            v3 <- normalize(complete[,ind, drop = FALSE])
          }
          else {
            stop(paste(transform, "is not a recognized transformation"))
          }
          nrmse <-  nrmse(v1, v2, v3)
          if (!is.null(nrmse)) err[1] <- mean(nrmse, na.rm = TRUE)
        }

      }
      else if (t == 'ordered' & !ignore.rank) {
        rankerror <- missrank(imputed[,ind, drop = FALSE],
                              incomplete[,ind, drop = FALSE],
                              complete[,ind, drop = FALSE])
        if (!is.null(rankerror))  err[4] <- mean(rankerror, na.rm = TRUE)
      }

      # ??? this does not mean that it is necessarily a factor variable. IMPROVE IT
      else {
        meanclasserr <- mean_per_class_error(imputed[,ind, drop = FALSE],
                                  incomplete[,ind, drop = FALSE],
                                  complete[,ind, drop = FALSE])
        if (!is.null(meanclasserr)) err[2] <- mean(meanclasserr, na.rm = TRUE)

        if (!ignore.missclass) {
          classerror <- missclass(imputed[,ind, drop = FALSE],
                                  incomplete[,ind, drop = FALSE],
                                  complete[,ind, drop = FALSE])
          if (!is.null(classerror)) err[3] <- mean(classerror, na.rm = TRUE)
        }
      }
    }

    if (varwise) {
      vwa <- NULL
      if (length(nrmse) > 1) vwa <- c(vwa, nrmse)
      else if (is.valid(nrmse)) vwa <- c(vwa, nrmse)
      if (ignore.missclass) {
        if (length(meanclasserr) > 1) vwa <- c(vwa, meanclasserr)
        else if (is.valid(meanclasserr)) vwa <- c(vwa, meanclasserr)
      }
      else {
        if (length(classerror) > 1) vwa <- c(vwa, classerror)
        else if (is.valid(classerror)) vwa <- c(vwa, classerror)
      }
      if (length(rankerror) > 1) vwa <- c(vwa, rankerror)
      else if (is.valid(rankerror)) vwa <- c(vwa, rankerror)
      return(list(error = err[!is.na(err)],
                  nrmse = nrmse,
                  missclass = classerror,
                  missrank = rankerror,
                  all = vwa))
    }
    else return(err[!is.na(err)])
  }

  else if ("mlim.mi" %in% class(imputed) |
           "list"    %in% class(imputed) |
           "mids"    %in% class(imputed)) {
    mat <- NULL
    for (i in 1:length(imputed)) {
      tmp <- imputed[[i]]
      class(tmp) <- c("mlim", "data.frame")
      mat <- rbind(mat, mlim.error(tmp, incomplete, complete,
                                   varwise = varwise,
                                   transform = transform,
                                   ignore.rank=ignore.rank))
    }
    return(mat)
  }
  else stop("'imputed' must be of class 'data.frame', 'list', 'mlim', 'mlim.mi', or 'mids'")
}

# data(charity)
# charity$ta1 <- factor(charity$ta1, ordered = FALSE)
# for (i in colnames(charity))
# dfNA <-  mlim.na(charity, p = 0.1, stratify = TRUE, seed = 2022)
# imp <- missRanger::missRanger(dfNA)
# mlim.error(imp, dfNA, charity)
#print((ELNETerror <- mlim.error(ELNET, dfNA, df)))

# if we standardize numeric vars, RMSE and MAE can be more than 1






