
#' @title check data.frame features and convert documents
#' @description extracts features classes
#' @param data data.frame
#' @param ordinal_as_integer model ordinal variable as gausian, but force
#'                           the imputed values to be integers. if FALSE, the
#'                           feature will be imputed as a multinomial factor
#' @return character vector of features types.
#' @examples
#' \dontrun{
#' data(iris)
#'
#' # add an integer variable to iris
#' iris$int <- as.integer(round(iris$Sepal.Length + iris$Sepal.Width))
#'
#' # add an integer variable to iris
#' iris$order <- factor(iris$Species, order = TRUE,
#'               levels = c("setosa","versicolor","virginica"))
#'
#' # add a binary variable to iris
#' iris$binary <- iris$Species
#' iris$binary[iris$binary=="versicolor"] <- "setosa"
#'
#' #print(checkNconvert(iris, ordinal_as_integer=TRUE))
#' }
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

checkNconvert <- function(data, vars2impute, ignore,
                          ordinal_as_integer=FALSE, md.log=NULL) {

  # Check all the variables in the data.frame, if not ignored
  # ============================================================
  if (!is.null(ignore)) usecols <- colnames(data)[! colnames(data) %in% ignore]
  else usecols <- colnames(data)

  ncl <- ncol(data[, usecols])
  features <- character(ncl)
  family <- character(ncl)
  classes <- lapply(data[, usecols], class)

  # get the vartype of the variables that should be imputed
  # convert incompatible variable types
  # ============================================================
  j <- 0
  for (i in usecols) {
    j <- j + 1
    # first evaluate the factors and numeric
    if ("factor" %in% classes[[i]])  {
      features[j] <- 'factor'
      cardinality <- length(unique(data[!is.na(data[, i]), i]))
      if (cardinality <= 2) family[j] <- 'binomial'
      else family[j] <- 'multinomial'
    }
    else if ("numeric" %in% classes[[i]]){
      features[j] <- 'numeric'
      family[j] <- 'gaussian'
    }

    # then search for ordinal & integers
    if ("ordered" %in% classes[[i]])  {
      features[j] <- 'ordered'
      cardinality <- length(unique(data[!is.na(data[, i]), i]))
      if (cardinality <= 2 & !ordinal_as_integer) {
        family[j] <- 'binomial'
        data[,i] <- factor(data[,i], ordered = FALSE)
      }
      else if (cardinality <= 2 & ordinal_as_integer) {
        family[j] <- 'quasibinomial'
        #  take the labels if numeric
        if (is.numeric(as.character(data[,i]))) {
          data[,i] <- as.numeric(levels(data[,i]))[data[,i]]
        }
        # otherwise take the levels
        else {
          data[,i] <- as.numeric(data[,i])
        }
      }
      else if (cardinality > 2 & !ordinal_as_integer) {
        family[j] <- 'multinomial'
        data[,i] <- factor(data[,i], ordered = FALSE)
      }
      else if (cardinality > 2 & ordinal_as_integer) {
        family[j] <- 'gaussian_integer'

        #  take the labels if numeric
        if (is.numeric(as.character(data[,i]))) {
          data[,i] <- as.numeric(levels(data[,i]))[data[,i]]
        }
        # otherwise take the levels
        else {
          data[,i] <- as.numeric(data[,i])
        }
      }
    }
    else if ("integer" %in% classes[[i]])  {
      features[j] <- 'integer'
      family[j] <- 'gaussian_integer'
      data[,i] <- as.numeric(data[,i])
    }
    else if ("character" %in% classes[[i]])  {
      msg <- print(paste(usecols[j], "variable is of class 'character', which",
                         "is not supported. either convert it to a 'factor'",
                         "or give it to the 'ignore' argument."))
      stop(msg)
    }
  }

  # only return the class and family of the vars2impute
  # ============================================================
  index <- which(usecols %in% vars2impute)

  return(list(class=features[index],
              family=family[index],
              data = data))
}


#iim <- selectVariables(data=irisWithNA, ignore = c("Sepal.Length","Sepal.Width" ,"Petal.Length","Petal.Width"))
#get <- checkNconvert(data=DATA1, vars2impute=vars2impute, ignore = NULL)
#print(get$family)
#print(get$class)

