#' @title Add artificial missing observations
#' @description Add MCAR, MAR, or MNAR missingness to evaluate imputation
#'              algorithms. Stratified sampling is available for MCAR only.
#' @param x An atomic vector or a data.frame (not a tibble).
#' @param p Expected proportion of missing observations per selected variable
#'          under MAR or MNAR; proportion used by addNA() under MCAR.
#' @param stratify Logical. Use stratified sampling for factor variables under MCAR.
#' @param classes Character vector of variable classes eligible for missingness.
#'                NULL selects all variables identified by the mechanism.
#' @param seed Integer. Random seed for reproducibility.
#' @param mechanism One of "mcar" (default), "mar", or "mnar".
#' @param mar Named numeric vector or coefficient matrix for observed predictors
#'            of missingness. For MAR, matrix rows identify incomplete variables
#'            and columns identify predictors; a vector applies to all variables
#'            except its predictors. With MNAR, this argument is optional and its
#'            nonzero predictors must remain fully observed.
#' @param mnar For mechanism = "mnar", a numeric coefficient applied to all
#'             eligible variables, a named numeric vector of variable-specific
#'             coefficients, or a named list. Numeric coefficients determine the
#'             effect of the variable's own standardized value on its probability
#'             of missingness. For unordered factors or character variables,
#'             provide a named vector of category-specific log-odds effects in
#'             a list, e.g., list(group = c(A = 0, B = 1, C = -1)).
#' @return A data.frame or atomic vector with artificial missing values.
#' @author E. F. Haghish
#' @examples
#' \dontrun{
#' data(iris)
#' mlim.na(iris, p = 0.15, seed = 1)
#' mlim.na(iris, p = 0.15, stratify = TRUE, classes = "factor", seed = 1)
#'
#' dat <- data.frame(v1 = rnorm(1000), v2 = rnorm(1000),
#'                   outcome = rnorm(1000))
#' mlim.na(dat, p = 0.15, mechanism = "mar", mar = c(outcome = 1), seed = 1)
#' mlim.na(dat, p = 0.15, mechanism = "mnar", mnar = 1, seed = 1)
#' mlim.na(dat, p = 0.15, mechanism = "mnar",
#'         mnar = c(v1 = 1, v2 = -1), seed = 1)
#' mlim.na(dat, p = 0.15, mechanism = "mnar",
#'         mnar = c(v1 = 1, v2 = -1), mar = c(outcome = 0.5), seed = 1)
#'
#' mixed <- data.frame(score = rnorm(1000),
#'                     group = factor(sample(c("A", "B", "C"), 1000, TRUE)))
#' mlim.na(mixed, p = 0.15, mechanism = "mnar",
#'         mnar = list(score = 1, group = c(A = 0, B = 1, C = -1)), seed = 1)
#' }
#' @export

mlim.na <- function(x, p = 0.1, stratify = FALSE, classes = NULL,
                    seed = NULL, mechanism = "mcar", mar = NULL, mnar = NULL) {

  # Syntax processing
  # ------------------------------------------------------------
  stopifnot(
    "'p' should be between 0 and 1" = p >= 0 & p <= 1,
    "'x' type is not recognized" = is.atomic(x) || is.data.frame(x),
    "class of 'x' must be strictly data.frame" =
      !any(class(x) %in% c("tbl", "tbl_df"))
  )

  mechanism <- match.arg(mechanism, c("mcar", "mar", "mnar"))

  # Set the seed for reproducibility.
  if (!is.null(seed)) set.seed(seed)

  # Add MNAR missingness based on the variable's own complete values.
  # An observed MAR predictor can also contribute to the probability.
  if (mechanism == "mnar") {
    if (is.null(mnar)) stop("MNAR requires 'mnar' coefficients.")
    if (stratify) stop("'stratify' is only available for MCAR.")

    # Atomic vectors are supported when a single MNAR coefficient is given.
    if (is.atomic(x)) {
      if (!is.null(mar) || !is.numeric(mnar) || length(mnar) != 1L ||
          !is.null(names(mnar))) {
        stop("For atomic vectors, supply one unnamed numeric 'mnar' coefficient and no 'mar'.")
      }
      result <- mlim.na(data.frame(value = x), p = p, classes = classes,
                        mechanism = "mnar", mnar = mnar)
      return(result[[1L]])
    }

    # A scalar applies to all selected variables; named specifications
    # identify which variables should receive missingness.
    scalar <- is.numeric(mnar) && length(mnar) == 1L && is.null(names(mnar))
    if (scalar) {
      if (is.matrix(mar)) {
        vars <- rownames(mar)
      } else {
        vars <- setdiff(names(x), if (is.null(mar)) character(0) else names(mar))
      }
      effects <- setNames(rep(list(mnar), length(vars)), vars)
    } else if ((is.numeric(mnar) || is.list(mnar)) &&
               !is.null(names(mnar)) && length(mnar) > 0L) {
      vars <- names(mnar)
      effects <- as.list(mnar)
    } else {
      stop("'mnar' must be a scalar, a named numeric vector, or a named list.")
    }

    if (is.null(vars) || anyNA(vars) || any(!nzchar(vars)) ||
        anyDuplicated(vars) || !all(vars %in% names(x))) {
      stop("MNAR variable names must be unique and present in 'x'.")
    }
    if (!is.null(classes)) {
      vars <- vars[vapply(x[vars], function(z) class(z)[1] %in% classes, logical(1))]
    }
    if (length(vars) == 0L || p == 0) return(x)
    if (p == 1) {
      for (v in vars) x[[v]][] <- NA
      return(x)
    }

    # Optional observed predictors of MNAR missingness.
    if (is.null(mar)) {
      coefficients <- matrix(0, nrow = length(vars), ncol = 0L,
                             dimnames = list(vars, character(0)))
    } else if (is.numeric(mar) && is.null(dim(mar)) &&
               !is.null(names(mar))) {
      coefficients <- matrix(rep(mar, each = length(vars)), nrow = length(vars),
                             dimnames = list(vars, names(mar)))
    } else if (is.matrix(mar) && is.numeric(mar) &&
               !is.null(rownames(mar)) && !is.null(colnames(mar))) {
      if (!all(rownames(mar) %in% vars)) {
        stop("Rows of 'mar' must identify MNAR target variables.")
      }
      coefficients <- matrix(0, nrow = length(vars), ncol = ncol(mar),
                             dimnames = list(vars, colnames(mar)))
      coefficients[rownames(mar), colnames(mar)] <- mar
    } else {
      stop("'mar' must be a named numeric vector or a coefficient matrix.")
    }

    if (anyNA(coefficients) || any(!is.finite(coefficients)) ||
        anyDuplicated(colnames(coefficients)) ||
        !all(colnames(coefficients) %in% names(x))) {
      stop("Invalid 'mar' coefficients or predictor names.")
    }
    drivers <- colnames(coefficients)[colSums(coefficients != 0) > 0]
    if (any(drivers %in% vars)) {
      stop("Observed 'mar' predictors cannot also receive MNAR missingness.")
    }
    if (any(vapply(x[drivers], anyNA, logical(1)))) {
      stop("Observed 'mar' predictors must not contain missing values.")
    }

    # Standardize numeric, binary, and ordered predictors.
    standardize <- function(z, name) {
      if (!(is.numeric(z) || is.logical(z) || is.ordered(z))) {
        stop("Variable '", name, "' must be numeric, logical, or ordered; ",
             "use named category effects for nominal MNAR targets.")
      }
      z <- as.numeric(z)
      s <- stats::sd(z)
      if (!is.finite(s) || s == 0) {
        stop("Variable '", name, "' has no variation for standardization.")
      }
      (z - mean(z)) / s
    }

    # Cache standardized fully observed predictors.
    observed <- lapply(drivers, function(d) standardize(x[[d]], d))
    names(observed) <- drivers

    # Generate independent missingness indicators for each target variable.
    for (v in vars) {
      z <- x[[v]]
      if (anyNA(z)) stop("MNAR target '", v, "' must be fully observed.")
      effect <- effects[[v]]
      if (!is.numeric(effect) || length(effect) == 0L || any(!is.finite(effect))) {
        stop("Invalid MNAR effect for '", v, "'.")
      }

      if (length(effect) == 1L && is.null(names(effect))) {
        if (effect == 0) {
          eta <- numeric(nrow(x))
        } else {
          eta <- effect * standardize(z, v)
        }
      } else {
        if (!(is.factor(z) || is.character(z)) || is.null(names(effect)) ||
            anyNA(names(effect)) || any(!nzchar(names(effect))) ||
            anyDuplicated(names(effect)) ||
            !all(unique(as.character(z)) %in% names(effect))) {
          stop("For categorical MNAR target '", v,
               "', provide named effects for every observed category.")
        }
        eta <- unname(effect[as.character(z)])
      }

      b <- coefficients[v, ]
      for (d in names(b)[b != 0]) eta <- eta + b[[d]] * observed[[d]]

      # Calibrate the intercept to the expected missingness proportion p.
      # These limits bracket the root even when coefficients are large.
      alpha <- stats::uniroot(
        function(a) mean(stats::plogis(a + eta)) - p,
        interval = c(stats::qlogis(p) - max(eta) - 1,
                     stats::qlogis(p) - min(eta) + 1)
      )$root
      x[[v]][stats::runif(nrow(x)) < stats::plogis(alpha + eta)] <- NA
    }
    return(x)
  }

  # Add MAR missingness using observed predictors of missingness.
  if (mechanism == "mar") {
    if (!is.data.frame(x) || is.null(mar)) {
      stop("MAR requires a data.frame and a named vector or matrix.")
    }
    if (stratify) stop("'stratify' is only available for MCAR.")

    # A vector applies the same model to all variables except its drivers.
    if (is.null(dim(mar))) {
      vars <- setdiff(names(x), names(mar))
      mar <- matrix(rep(mar, each = length(vars)), nrow = length(vars),
                    dimnames = list(vars, names(mar)))
    }

    vars <- rownames(mar)
    if (is.null(vars) || is.null(colnames(mar))) {
      stop("MAR matrix rows and columns must have variable names.")
    }
    if (!is.null(classes)) {
      vars <- vars[vapply(x[vars], function(z) {
        class(z)[1] %in% classes
      }, logical(1))]
    }
    if (p == 0) return(x)
    if (p == 1) {
      for (v in vars) x[[v]][] <- NA
      return(x)
    }

    for (i in seq_along(vars)) {
      v <- vars[i]
      b <- mar[v, ]
      drivers <- names(b)[b != 0]

      # A MAR driver must remain observed, or have been processed earlier.
      if (any(drivers %in% vars[i:length(vars)])) {
        stop("MAR cannot depend on current or later missing variables.")
      }

      eta <- numeric(nrow(x))
      for (d in drivers) {
        observed <- !is.na(x[[d]])
        eta[observed] <- eta[observed] +
          b[d] * as.numeric(scale(x[[d]][observed]))
      }

      # Calibrate the expected missingness rate for each variable to p.
      alpha <- stats::uniroot(
        function(a) mean(stats::plogis(a + eta)) - p,
        interval = c(-40, 40)
      )$root
      x[[v]][stats::runif(nrow(x)) < stats::plogis(alpha + eta)] <- NA
    }
    return(x)
  }

  # Original MCAR sampling behavior, including atomic vectors and stratification.
  if (is.atomic(x)) {
    x <- addNA(x, p, stratify = stratify)
    return(x)
  } else {
    for (i in colnames(x)) {
      if (is.null(classes)) x[, i] <- addNA(x[, i], p, stratify = stratify)
      else {
        if (class(x[, i, drop = TRUE])[1] %in% classes) {
          x[, i] <- addNA(x[, i], p, stratify = stratify)
        }
      }
    }
  }

  return(x)
}
