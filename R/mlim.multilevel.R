#' @title Prepare Multilevel Data for Imputation
#' @description Adds cluster-level summary variables and automatically reshapes
#'   long hierarchical data to wide format.
#' @param data A data frame containing variables to be imputed and hierarchy IDs.
#' @param hierarchy Character vector of clustering variables from highest to
#'   lowest level, for example `c("school", "student")`.
#' @param variables Variables for which cluster summaries are generated. By
#'   default, all non-hierarchy variables are used.
#' @param leave_one_out Logical. If `TRUE`, each row is excluded from its own
#'   cluster summary. The default is `TRUE`.
#' @param add_cluster_size Logical. If `TRUE`, cluster size is added at each
#'   informative hierarchy level. The default is `TRUE`.
#' @param weights Optional non-negative observation weights.
#' @details Data format is detected automatically. Repeated combinations of the
#'   hierarchy variables indicate long data; unique combinations indicate data
#'   that are already wide. Long data are reshaped so that one row represents
#'   one unit at the lowest hierarchy level. Occasion numbers follow row order.
#'   Mapping metadata are stored so `mlim.multilevel.long()` can restore the
#'   original long rows.
#' @importFrom stats ave reshape
#' @return A wide data frame with generated multilevel predictors.
#' @examples
#' if (requireNamespace("mlmRev", quietly = TRUE)) {
#'   data("egsingle", package = "mlmRev")
#'   dat <- egsingle
#'   set.seed(123)
#'   dat$math[sample(seq_len(nrow(dat)), 500)] <- NA
#'   wide <- mlim.multilevel(dat, c("schoolid", "childid"), variables = "math")
#'   restored <- mlim.multilevel.long(wide)
#' }
#' @author E. F. Haghish
#' @keywords internal
#' @noRd
mlim.multilevel <- function(data, hierarchy, variables = NULL, leave_one_out = TRUE,
                            add_cluster_size = TRUE, weights = NULL) {

  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!is.character(hierarchy) || !length(hierarchy)) stop("'hierarchy' must be a character vector.")
  if (!all(hierarchy %in% names(data))) {
    stop("Hierarchy variables not found in data: ", paste(setdiff(hierarchy, names(data)), collapse = ", "))
  }
  if (anyNA(data[hierarchy])) stop("Hierarchy variables cannot contain missing values.")

  if (is.null(weights)) weights <- rep(1, nrow(data))
  if (!is.numeric(weights) || length(weights) != nrow(data) || anyNA(weights) ||
      any(!is.finite(weights)) || any(weights < 0)) {
    stop("'weights' must be a non-negative numeric vector with one finite value per row.")
  }

  if (is.null(variables)) variables <- setdiff(names(data), hierarchy)
  if (!all(variables %in% names(data))) stop("Some variables specified in 'variables' are not in the data.")

  input_format <- if (anyDuplicated(data[hierarchy]) > 0L) "long" else "wide"
  original_variables <- names(data)

  cluster_mean <- function(x, group, w, loo) {
    observed <- !is.na(x)
    total <- ave(ifelse(observed, w * x, 0), group, FUN = sum)
    n <- ave(ifelse(observed, w, 0), group, FUN = sum)
    if (loo) {
      total <- total - ifelse(observed, w * x, 0)
      n <- n - ifelse(observed, w, 0)
    }
    out <- total / n
    out[n == 0] <- NA_real_
    out
  }

  cluster_prop <- function(x, level, group, w, loo) {
    observed <- !is.na(x)
    indicator <- ifelse(observed, as.integer(x == level), 0)
    total <- ave(w * indicator, group, FUN = sum)
    n <- ave(ifelse(observed, w, 0), group, FUN = sum)
    if (loo) {
      total <- total - ifelse(observed, w * indicator, 0)
      n <- n - ifelse(observed, w, 0)
    }
    out <- total / n
    out[n == 0] <- NA_real_
    out
  }

  out <- data
  for (i in seq_along(hierarchy)) {
    grouping_vars <- hierarchy[seq_len(i)]
    group <- do.call(interaction, c(data[grouping_vars], list(drop = TRUE, lex.order = TRUE)))
    level_name <- paste(grouping_vars, collapse = "_")
    cluster_n <- ave(weights, group, FUN = sum)
    if (all(cluster_n == 1)) next
    if (add_cluster_size) out[[paste0("_", level_name, "_n")]] <- cluster_n

    for (variable in variables) {
      x <- data[[variable]]
      prefix <- paste0("_", level_name, "_", variable)

      if (is.numeric(x) && !is.factor(x)) {
        out[[paste0(prefix, "_mean")]] <- cluster_mean(x, group, weights, leave_one_out)
      } else if (is.ordered(x)) {
        out[[paste0(prefix, "_mean")]] <- cluster_mean(as.numeric(x), group, weights, leave_one_out)
      } else if (is.factor(x) && length(levels(x)) == 2L) {
        out[[paste0(prefix, "_prop")]] <- cluster_prop(x, levels(x)[2L], group, weights, leave_one_out)
      } else if (is.factor(x) || is.character(x)) {
        x <- factor(x)
        for (lev in levels(x)[-1L]) {
          out[[paste0(prefix, "_prop_", make.names(lev))]] <- cluster_prop(x, lev, group, weights, leave_one_out)
        }
      }
    }
  }

  multilevel_variables <- setdiff(names(out), original_variables)

  if (input_format == "wide") {
    attr(out, "mlim.hierarchy") <- hierarchy
    attr(out, "mlim.original.variables") <- original_variables
    attr(out, "mlim.multilevel.variables") <- multilevel_variables
    attr(out, "mlim.multilevel.format") <- "wide"
    attr(out, "mlim.multilevel.input.format") <- "wide"
    return(out)
  }

  reserved <- c(".mlim_occasion", ".mlim_present", ".mlim_unit_order", ".mlim_row_id", ".mlim_unit_key")
  if (any(reserved %in% names(out))) {
    stop("The data contain a variable name reserved for the internal wide multilevel transformation.")
  }

  make_unit_key <- function(df) do.call(paste, c(lapply(df, as.character), list(sep = "\u001f")))
  unit_key <- make_unit_key(data[hierarchy])
  unit_order <- match(unit_key, unique(unit_key))
  occasion <- as.integer(ave(seq_len(nrow(out)), unit_order, FUN = seq_along))
  times <- seq_len(max(occasion))

  mapping <- data.frame(.mlim_row_id = seq_len(nrow(data)), .mlim_unit_order = unit_order,
                        .mlim_unit_key = unit_key, .mlim_occasion = occasion,
                        data[hierarchy], check.names = FALSE)

  payload <- setdiff(names(out), hierarchy)
  wide_source <- out
  wide_source$.mlim_occasion <- occasion
  wide_source$.mlim_present <- 1L
  wide_source$.mlim_unit_order <- unit_order

  wide <- stats::reshape(wide_source, idvar = c(hierarchy, ".mlim_unit_order"),
                         timevar = ".mlim_occasion", direction = "wide",
                         v.names = c(payload, ".mlim_present"), sep = "__t")
  wide <- wide[order(wide$.mlim_unit_order), , drop = FALSE]
  rownames(wide) <- NULL

  present_names <- paste0(".mlim_present__t", times)
  present <- !is.na(as.matrix(wide[, present_names, drop = FALSE]))
  colnames(present) <- paste0("t", times)
  wide$.mlim_unit_order <- NULL
  wide[present_names] <- NULL

  column_map <- setNames(lapply(payload, function(v) paste0(v, "__t", times)), payload)
  wide_multilevel_variables <- unlist(column_map[multilevel_variables], use.names = FALSE)

  attr(wide, "mlim.hierarchy") <- hierarchy
  attr(wide, "mlim.original.variables") <- original_variables
  attr(wide, "mlim.multilevel.variables") <- wide_multilevel_variables
  attr(wide, "mlim.multilevel.long.variables") <- multilevel_variables
  attr(wide, "mlim.multilevel.format") <- "wide"
  attr(wide, "mlim.multilevel.input.format") <- "long"
  attr(wide, "mlim.wide.mapping") <- mapping
  attr(wide, "mlim.wide.times") <- times
  attr(wide, "mlim.wide.column.map") <- column_map
  attr(wide, "mlim.wide.present") <- present
  attr(wide, "mlim.wide.long.columns") <- names(out)
  wide
}
