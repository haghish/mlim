#' @title get the next variable in the iteration to impute
#' @description this function evaluates the saved mlim object and
#'              decides which loop and iteration to carry out next.
#' @param m integer. number of imputation datasets
#' @param m.it integer. current number of dataset that is being imputed
#' @param k integer. current global loop iteration. each dataset goes up to
#'          'maxiter' number of global iterations
#' @param z integer. current number of imputation variable in the 'ITERATIONVARS'.
#'          this specifies the local iteration number.
#' @param Y character. name of the variable that is to be imputed (or was lastly
#'          imputed, in case of loading mlim object)
#' @param ITERATIONVARS character vector, specifying variable names to be imputed
#' @param maxiter maximum number of global iterations for  each dataset
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
iterationNextVar <- function(m, m.it, k, z, Y, ITERATIONVARS, maxiter) {

  # if 'z' is smaller than the length of 'ITERATIONVARS', move-on to next variable
  # if 'z' is the last in the iteration, first check the iteration limits
  #     if iterations limit is reached, check the MI datasets limit
  #          if there is a place for multiple imputation, move on to next data
  #             and reset the iterations
  #          else stop the imputation
  #     else move on to the next global loop iteration
  # if there is a continuation, specify the next imputation variable

  if (z < length(ITERATIONVARS)) {
    z <- z + 1
  }
  else if (z == length(ITERATIONVARS)) {
    if (k == maxiter) {
      if (m.it < m) {
        m.it <- m.it + 1
        k <- 1
        z <- 1
      }
      else stop("all datasets were already generated!")
    }
    else {
      k <- k + 1
      z <- 1
    }
  }

  # update the next variable to be imputed
  Y <- ITERATIONVARS[z]

  return(list(m=m,
              m.it=m.it,
              k=k,
              z=z,
              Y=Y))
}

# print(paste(object$m, object$m.it, object$k, object$z, object$Y, object$ITERATIONVARS, object$maxiter))
# iterationNextVar(object$m, object$m.it, object$k, object$z, object$Y, object$ITERATIONVARS, object$maxiter)
