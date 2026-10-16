#' Hierarchy-aware unit definitions for mlim
#'
#' A target declared at a higher level contributes exactly one observation
#' per independent unit to the training and validation tasks. Lower-level
#' predictors are summarized by the mean (numeric) or mode (categorical).
#' These deterministic reductions are exploratory, not a guarantee of MAR or
#' congenial multiple imputation. They must never use the target's unobserved
#' values as training labels.
#' @keywords internal
#' @noRd

mlim.variable_levels.key <- function(data, hierarchy, level) {
  j <- match(level, hierarchy)
  if (is.na(j)) stop("Unknown variable level: ", level, call. = FALSE)
  if (anyNA(data[hierarchy[seq_len(j)]])) {
    stop("Hierarchy identifiers cannot be missing.", call. = FALSE)
  }
  do.call(paste, c(lapply(data[hierarchy[seq_len(j)]], as.character),
                   list(sep = "\037")))
}

mlim.variable_levels.validate <- function(data, hierarchy, variable_levels) {
  if (is.null(variable_levels) || !length(variable_levels)) return(NULL)
  if (!is.list(variable_levels) || is.null(names(variable_levels)) ||
      anyNA(names(variable_levels)) || any(!nzchar(names(variable_levels))) ||
      anyDuplicated(names(variable_levels))) {
    stop("'variable_levels' must be a named list, e.g. list(schoolid = c('size','mobility')).",
         call. = FALSE)
  }
  if (is.null(hierarchy) || !is.character(hierarchy) ||
      !length(hierarchy) || !all(names(variable_levels) %in% hierarchy)) {
    stop("Every name in 'variable_levels' must occur in 'hierarchy'.", call. = FALSE)
  }
  vars <- unlist(variable_levels, use.names = FALSE)
  if (!is.character(vars) || anyNA(vars) || any(!nzchar(vars)) ||
      anyDuplicated(vars) || !all(vars %in% names(data)) ||
      any(vars %in% hierarchy)) {
    stop("Variables in 'variable_levels' must be existing, distinct, non-hierarchy columns.",
         call. = FALSE)
  }
  # Ensure the unmasked observations represent one actual variable per unit.
  # This must run BEFORE random/mode preimputation alters the missingness.
  for (level in names(variable_levels)) for (v in variable_levels[[level]]) {
    key <- mlim.variable_levels.key(data, hierarchy, level)
    parts <- split(seq_along(key), factor(key, levels = unique(key)))
    bad <- vapply(parts, function(idx) {
      values <- unique(as.character(stats::na.omit(data[[v]][idx])))
      length(values) > 1L
    }, logical(1))
    if (any(bad)) {
      stop("Variable '", v, "' was declared at '", level,
           "' but contains conflicting observed values within ", sum(bad),
           " unit(s). Correct the data or change its declared level.", call. = FALSE)
    }
  }
  variable_levels
}

# Enforce one initial imputation per completely unobserved unit. For partially
# observed units, propagate the observed value instead of randomizing rows.
mlim.variable_levels.preimpute <- function(original, initialized,
                                           variable_levels, hierarchy) {
  if (is.null(variable_levels)) return(initialized)
  for (level in names(variable_levels)) {
    key <- mlim.variable_levels.key(original, hierarchy, level)
    parts <- split(seq_along(key), factor(key, levels = unique(key)))
    for (v in variable_levels[[level]]) {
      for (idx in parts) {
        known <- idx[!is.na(original[[v]][idx])]
        value <- if (length(known)) original[[v]][known[1L]] else initialized[[v]][idx[1L]]
        initialized[[v]][idx] <- value
      }
    }
  }
  initialized
}

# Reconcile final integer matching, which otherwise operates on individual rows.
mlim.variable_levels.reconcile <- function(data, missing, variable_levels, hierarchy) {
  if (is.null(variable_levels)) return(data)
  for (level in names(variable_levels)) {
    key <- mlim.variable_levels.key(data, hierarchy, level)
    parts <- split(seq_along(key), factor(key, levels = unique(key)))
    for (v in intersect(variable_levels[[level]], colnames(missing))) {
      mask <- as.logical(missing[, v])
      for (idx in parts) {
        known <- idx[!mask[idx]]
        value <- data[[v]][if (length(known)) known[1L] else idx[1L]]
        data[[v]][idx[mask[idx]]] <- value
      }
    }
  }
  data
}

mlim.variable_levels.collapse <- function(data, missing, target, hierarchy, level) {
  key <- mlim.variable_levels.key(data, hierarchy, level)
  units <- unique(key)
  index <- match(key, units)
  parts <- split(seq_along(index), factor(index, levels = seq_along(units)))
  representative <- vapply(parts, `[`, integer(1), 1L)
  out <- data[representative, , drop = FALSE]
  rownames(out) <- NULL
  observed <- !as.logical(missing[, target])
  observed_by_unit <- vapply(parts, function(idx) any(observed[idx]), logical(1))
  mask <- !observed_by_unit
  # Aggregating predictors deliberately excludes the target and IDs. Numeric
  # columns are averaged; factors / strings take modal values. This keeps the
  # same unit-level predictor for the training and imputation sets.
  excluded <- c(target, hierarchy, "mlim_bootstrap_weights_column_",
                "mlim_model_weights_column_")
  for (v in setdiff(names(data), excluded)) {
    x <- data[[v]]
    if (is.numeric(x) && !is.factor(x)) {
      out[[v]] <- vapply(parts, function(idx) mean(x[idx], na.rm = TRUE), numeric(1))
    } else if (is.factor(x) || is.character(x) || is.logical(x)) {
      for (i in seq_along(parts)) {
        vals <- x[parts[[i]]]
        vals <- vals[!is.na(vals)]
        if (length(vals)) {
          tbl <- table(as.character(vals))
          out[[v]][i] <- vals[match(names(tbl)[which.max(tbl)], as.character(vals))]
        }
      }
    }
  }
  # Original values take precedence for a partially observed unit. A fully
  # missing unit retains its level-consistent initialized target temporarily.
  for (i in seq_along(parts)) {
    idx <- parts[[i]]
    known <- idx[observed[idx]]
    if (length(known)) out[[target]][i] <- data[[target]][known[1L]]
  }
  if (anyNA(out)) {
    stop("Level-aware aggregation generated NA values; check preprocessing and predictor types.",
         call. = FALSE)
  }
  list(data = out, mask = mask, row_index = index, unit_keys = units)
}

mlim.variable_levels.prepare <- function(data, bdata, dataNA, bdataNA,
                                         target, hierarchy, level) {
  regular <- mlim.variable_levels.collapse(data, dataNA, target, hierarchy, level)
  bootstrap <- if (is.null(bdata)) NULL else
    mlim.variable_levels.collapse(bdata, bdataNA, target, hierarchy, level)
  target_hierarchy <- hierarchy[seq_len(match(level, hierarchy) - 1L)]
  list(data = regular$data, bdata = if (is.null(bootstrap)) NULL else bootstrap$data,
       dataNA = matrix(regular$mask, ncol = 1L, dimnames = list(NULL, target)),
       bdataNA = if (is.null(bootstrap)) NULL else
         matrix(bootstrap$mask, ncol = 1L, dimnames = list(NULL, target)),
       data_rows = regular$row_index,
       bdata_rows = if (is.null(bootstrap)) NULL else bootstrap$row_index,
       hierarchy = target_hierarchy)
}

mlim.variable_levels.restore <- function(original, aggregate, row_index, target,
                                         original_mask) {
  if (is.null(original)) return(NULL)
  values <- aggregate[[target]][row_index]
  if (length(values) != nrow(original)) stop("Level-aware imputation lost row alignment.")
  original[[target]][as.logical(original_mask[, target])] <-
    values[as.logical(original_mask[, target])]
  original
}
