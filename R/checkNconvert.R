
#' @title check data.frame features and convert documents
#' @description extracts features classes
#' @param data data.frame
#' @param ignore.rank (RECOMMENDED) model ordinal variable as gausian, but force
#'                    the imputed values to be integers and are reverted later. if FALSE, the
#'                    feature will be imputed as a multinomial factor.
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
#' #print(checkNconvert(iris))
#' }
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
checkNconvert <- function(data, vars2impute, ignore,
                          ignore.rank=FALSE, report=NULL) {

  mem <- NULL
  orderedIndex <- 0

  COLNAMES <- colnames(data)
  COLNAMES <- COLNAMES[!COLNAMES %in% ignore]

  ncl <- length(COLNAMES)
  features <- character(ncl)
  family <- character(ncl)
  classes <- lapply(data[, COLNAMES, drop = FALSE], class)


  # get the vartype of the variables that should be imputed
  # convert incompatible variable types
  # ============================================================
  j <- 0

  for (i in COLNAMES) {
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
      if (cardinality <= 2 & ignore.rank) {
        family[j] <- 'binomial'
        data[,i] <- factor(data[,i], ordered = FALSE)
      }
      else if (cardinality <= 2 & !ignore.rank) {
        orderedIndex <- orderedIndex + 1
        mem[[orderedIndex]] <- factmem(data[, i, drop = FALSE])
        family[j] <- 'quasibinomial'
        # represent ordered factors by rank indices
        data[,i] <- as.numeric(data[,i])
      }
      else if (cardinality > 2 & ignore.rank) {
        family[j] <- 'multinomial'
        data[,i] <- factor(data[,i], ordered = FALSE)
      }
      else if (cardinality > 2 & !ignore.rank) {
        orderedIndex <- orderedIndex + 1
        mem[[orderedIndex]] <- factmem(data[, i, drop = FALSE])
        family[j] <- 'gaussian_integer'

        # represent ordered factors by rank indices
        data[,i] <- as.numeric(data[,i])
      }
    }
    else if ("integer" %in% classes[[i]])  {
      features[j] <- 'integer'
      family[j] <- 'gaussian_integer'
      data[,i] <- as.numeric(data[,i])
    }
    else if ("character" %in% classes[[i]])  {
      msg <- paste(i, "variable is of class 'character', which",
                   "is not supported. either convert it to a 'factor'",
                   "or give it to the 'ignore' argument.")
      stop(msg)
    }
  }

  # only return the class and family of the vars2impute, if not ignored
  # ============================================================
  # now match the location of the variables according to vars2impute order
  index <- match(vars2impute, COLNAMES)
  return(list(class = features[index],
              family = family[index],
              COLNAMES = COLNAMES[index],
              data = data,
              mem = mem,
              orderedCols = match(
                COLNAMES[features == "ordered"],
                colnames(data))
  )
  )
}

