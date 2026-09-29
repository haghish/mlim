#' @title assigns bootstrap data to cros validation groups
#' @description identifies duplicated rows in the bootstrap and creates
#'              a column in the bootstrap data assigning the cross-validation
#'              groups to avoid having duplicates in the training and testing
#' @author E. F. Haghish
#' @param index numeric. bootstrapped rows numbers from the original dataset
#' @param cv numeric. number of Cross-Validation (CV)
#' @return numeric data.frame including 'CV' amount of columns,
#'         owhere each column shows the row number of observations
#'         that belong to that CV group
#' @keywords Internal
#' @noRd
bootstrapCV <- function(index, cv = 10) {
  index <- sort(index)
  weight <- table(index)
  rows <- as.numeric(names(weight))
  tab <- as.data.frame(cbind(rows, weight))
  tab <- tab[order(tab$weight, decreasing = TRUE), ]

  # calculate the length of each group and start the groups
  col <- length(index) %/% cv
  mat <- as.data.frame(matrix(data = rep(NA, length(index)), ncol = cv))

  #distribute the values that have duplications
  tab.dup <- tab[tab$weight > 1, ]
  tab.unq <- tab[tab$weight == 1, ]
  rows <- tab.unq$rows

  j <- 1
  reverse <- FALSE
  for (i in 1:nrow(tab.dup)) {
    # which cv group should the rows be assigned to
    if (j > cv) {
      reverse <- TRUE
      j <- cv
    }
    if (j < 1) {
      reverse <- FALSE
      j <- 1
    }
    val <- rep(tab.dup[i,1], tab.dup[i,2])
    place <- which(is.na(mat[,j]))
    replace <- place[1]:(place[1]+length(val)-1)
    mat[replace,j] <- val

    if (!reverse) j <- j + 1
    else j <- j - 1
  }

  # Filling the remaining places with random rows
  # =============================================
  for (j in 1:cv) {
    place <- which(is.na(mat[,j]))
    replace <- place[1]:(nrow(mat))

    if (j < cv) {
      if (length(replace) >= 1) {
        val <- sample(rows, size = length(replace), replace = FALSE)
        mat[replace, j] <- val
        rows <- rows[! rows %in% val]
      }
    }
    # filling the last column, if it has any empty rows
    else {
      if (length(replace) >= 1) {
        if (length(rows) >= 1) {
          replace <- place[1]:(place[1]+length(rows)-1)
          mat[replace,j] <- rows
        }
      }
    }
  }
  return(mat)
}

# sampling_index <- sample.int(150, 145, replace=TRUE)
# sampling_index <- sort(sampling_index, decreasing = TRUE)
# View(bootstrapCV(sampling_index, cv = 5))
