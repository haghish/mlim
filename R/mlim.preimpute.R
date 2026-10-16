#' @title Preimputation of missing values
#' @description Initialize missing values by random sampling, median/mode, or
#'   experimental random forest. When variable_levels is specified, declared
#'   higher-level variables receive ONE initial value per school/child unit;
#'   partially observed units use their observed value throughout.
#'   With hierarchy and variable_levels, random and mm AUTOMATICALLY operate
#'   on distinct observed units for higher-level variables, rather than
#'   weighting donors or medians/modes by the number of repeated rows.
#'   Undeclared variables keep observation-level initialization. The RF method
#'   and its post-initialization unit reconciliation are unchanged.
#' @param data A data.frame containing incomplete observations.
#' @param preimpute Character: "random", "mm", or experimental "rf".
#' @param seed Optional random seed.
#' @param variable_levels Optional named list assigning higher-level variables to
#'   the hierarchy identifier at which they are constant. For example,
#'   list(schoolid = c('size', 'lowinc'), childid = 'female'). These declarations
#'   automatically activate level-aware random or median/mode preimputation.
#'   If NULL, all variables are initialized at the observation level.
#' @param hierarchy Ordered vector of hierarchy identifiers, highest first.
#' @importFrom missRanger missRanger imputeUnivariate
#' @return Preimputed data.frame.
#' @author E. F. Haghish
#' @export
mlim.preimpute <- function(data, preimpute = "random", seed = NULL,
                           variable_levels = NULL, hierarchy = NULL) {
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!is.character(preimpute) || length(preimpute) != 1L)
    stop("'preimpute' must be a single character value.")
  preimpute <- tolower(preimpute)
  if (!preimpute %in% c("rf", "mm", "random"))
    stop("'preimpute' must be 'rf', 'mm', or 'random'.")
  if (!is.null(variable_levels))
    variable_levels <- mlim.variable_levels.validate(data, hierarchy, variable_levels)
  original <- if (is.null(variable_levels)) NULL else data
  # When declared, higher-level targets are initialized at the independent
  # unit level by default (random / mm), never first imputed row by row.
  unit_vars <- if (preimpute != "rf" && !is.null(variable_levels))
    unlist(variable_levels, use.names = FALSE) else character(0)
  row_vars <- setdiff(names(data), unit_vars)

  if (preimpute == "rf") {
    message("\nPreimputation: Random Forest")
    pb <- utils::txtProgressBar(0, 1, style = 3)
    data <- missRanger::missRanger(data, num.trees = 500, mtry = 1,
                                   verbose = 0, returnOOB = TRUE, seed = seed)
    utils::setTxtProgressBar(pb, 1)
  } else if (preimpute == "mm") {
    message("\nPreimputation: Median/Mode")
    pb <- utils::txtProgressBar(0, 1, style = 3)
    if (length(unit_vars)) {
      if (length(row_vars)) data[row_vars] <- medianmode(data[row_vars])
    } else data <- medianmode(data)
    utils::setTxtProgressBar(pb, 1)
  } else {
    message("\nPreimputation: Random Sampling")
    if (!is.null(seed)) set.seed(seed)
    rsample <- function(x) {
      missing <- is.na(x)
      observed <- x[!missing]
      if (!length(observed)) stop("Cannot randomly preimpute a completely missing variable.")
      # sample.int() avoids sample(c(5), n) accidentally sampling 1:5.
      x[missing] <- observed[sample.int(length(observed), sum(missing), replace = TRUE)]
      x
    }
    pb <- utils::txtProgressBar(0, 1, style = 3)
    for (i in row_vars) if (anyNA(data[[i]])) data[[i]] <- rsample(data[[i]])
    utils::setTxtProgressBar(pb, 1)
  }
  if (length(unit_vars)) {
    data <- mlim.preimpute.units(original, data, variable_levels, hierarchy, preimpute)
  } else if (!is.null(variable_levels)) {
    # Preserve the existing post-initialization reconciliation, including RF.
    data <- mlim.variable_levels.preimpute(original, data, variable_levels, hierarchy)
  }
  data
}

# Initialize declared school/child-level columns directly at the unit level.
# Distinct donors are defined by cumulative hierarchy keys, preventing e.g.
# childid=1 in two different schools from being treated as the same child.
# Only observed values from the original incomplete data enter the donor pool.
# One donor per unique unit; all missing rows of an observed unit inherit its
# observed value, and a fully missing unit receives just one common draw.
#' @keywords Internal
#' @noRd
mlim.preimpute.units <- function(original, initialized, variable_levels, hierarchy, method) {
  for (level in names(variable_levels)) {
    key <- mlim.variable_levels.key(original, hierarchy, level)
    groups <- split(seq_along(key), factor(key, levels = unique(key)))
    for (v in variable_levels[[level]]) {
      x <- original[[v]]
      first_observed <- vapply(groups, function(idx) {
        observed <- idx[!is.na(x[idx])]
        if (length(observed)) observed[1L] else NA_integer_
      }, integer(1))
      donors <- x[first_observed[!is.na(first_observed)]]  # One value per observed unit
      if (!length(donors)) stop("Cannot preimpute '", v, "': no observed ", level, " units.", call. = FALSE)

      empty <- which(is.na(first_observed))
      draws <- if (!length(empty)) donors[integer(0)] else if (method == "random") {
        donors[sample.int(length(donors), length(empty), replace = TRUE)]
      } else if (is.numeric(donors) && !is.factor(donors)) {
        rep(stats::median(donors), length(empty))
      } else {
        # The modal donor retains the original factor/ordered-factor class.
        counts <- table(as.character(donors))
        mode_value <- donors[match(names(counts)[which.max(counts)], as.character(donors))]
        rep(mode_value, length(empty))
      }

      for (i in seq_along(groups)) {
        # A partially observed unit keeps its original observed value.
        value <- if (!is.na(first_observed[i])) x[first_observed[i]] else draws[match(i, empty)]
        initialized[[v]][groups[[i]]] <- value
      }
    }
  }
  initialized
}
