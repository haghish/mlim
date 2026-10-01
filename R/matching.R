#' @title Match imputed values to observed values
#' @description Matches imputed values to the observed support of a variable.
#'   For integer-valued variables, fractional predictions are stochastically
#'   matched between the two bounding observed values. For non-integer-valued
#'   variables, each imputed value is matched to the nearest observed value.
#' @param imputed numeric vector of imputed values
#' @param observed numeric vector of non-missing observed values
#' @return numeric vector of matched imputed values
#' @importFrom stats runif
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

matching <- function(imputed, observed) {
  
  if (is.null(imputed) || is.null(observed)) {
    return(imputed)
  }
  
  if (!is.numeric(imputed)) {
    stop("'imputed' must be numeric.", call. = FALSE)
  }
  
  if (!is.numeric(observed)) {
    stop("'observed' must be numeric.", call. = FALSE)
  }
  
  # Remove missing values from the observed support
  observed <- observed[!is.na(observed)]
  
  if (length(observed) == 0L) {
    stop("'observed' must contain at least one non-missing value.", call. = FALSE)
  }
  
  if (any(!is.finite(observed))) {
    stop("'observed' must contain finite values only.",call. = FALSE)
  }
  
  if (any(!is.finite(imputed[!is.na(imputed)]))) {
    stop("'imputed' contains non-finite values.", call. = FALSE)
  }
  
  # Get the unique observed support
  observed <- sort(unique(observed))
  
  # Determine whether the variable is integer-valued. This is better than is.integer()
  # because is.integer only checks how the data is stored, rather than the actual mathematical value...
  integer_variable <- all(observed == floor(observed))
  
  # Only non-missing imputed values need to be matched
  valid <- !is.na(imputed)
  
  if (!any(valid)) {
    return(imputed)
  }
  
  # ------------------------------------------------------------
  # Integer-valued variables:
  # stochastic matching between bounding observed values
  # ------------------------------------------------------------
  if (integer_variable) {
    for (i in which(valid)) {
      x <- imputed[i]
      
      # Already an observed value: keep it
      if (x %in% observed) {
        next
      }
      
      # Find the observed values immediately below and above x
      lower <- observed[observed < x]
      upper <- observed[observed > x]
      
      # Below the observed range
      if (length(lower) == 0L) {
        imputed[i] <- observed[1L]
        next
      }
      
      # Above the observed range
      if (length(upper) == 0L) {
        imputed[i] <- observed[length(observed)]
        next
      }
      
      lower <- max(lower)
      upper <- min(upper)
      
      # Probability of selecting the upper value.
      # This preserves the expected value:
      #
      # E(Y) = lower * (1-p) + upper * p = x.  (i.e., lower with probability of 1-p and upper with the probability of p)
      # this means that in the stochastic rounding, the average of the rounded values
      # equals the original model prediction!
      p_upper <- (x - lower) / (upper - lower)
      
      if (runif(1L) < p_upper) {
        imputed[i] <- upper
      }
      else {
        imputed[i] <- lower
      }
    }
    
  }
  
  # ------------------------------------------------------------
  # Non-integer-valued variables:
  # retain the original nearest-observed-value approach
  # ------------------------------------------------------------
  else {
    for (i in which(valid)) {
      distance <- abs(observed - imputed[i])
      nearest <- observed[distance == min(distance)]
      
      # If there is a tie, select the lower observed value
      imputed[i] <- min(nearest)
    }
  }
  
  return(imputed)
}
