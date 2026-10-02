#' @title add artificial missing observations at a hierarchical level
#' @description to examine the performance of imputation algorithms in
#'              hierarchical data, artificial missing data can be added at a
#'              selected level of the hierarchy. instead of sampling individual
#'              rows, this function samples complete clusters and sets the
#'              selected variables to NA for all observations belonging to the
#'              sampled clusters. because complete clusters are removed, the
#'              requested missing proportion is treated as a target and may not
#'              be achieved exactly.
#' @param x data.frame. x must be strictly a data.frame and any other
#'          data.table classes will be rejected
#' @param p target proportion of currently observed values to be replaced by NA
#'          in the selected variables. because complete clusters are sampled,
#'          the achieved proportion can differ slightly from 'p'.
#' @param hierarchy character vector specifying the clustering variables from
#'                   the highest to the lowest level. for example,
#'                   'hierarchy = c("schoolid", "childid")' specifies children
#'                   nested within schools.
#' @param level character string specifying the hierarchical level at which
#'              missingness should be generated. 'level' must name one variable
#'              in 'hierarchy'. for example, 'level = "childid"' samples
#'              complete children, whereas 'level = "schoolid"' samples
#'              complete schools.
#' @param variables character vector specifying the variables that should
#'                  receive NA values. the default value is NULL, meaning all
#'                  variables not included in 'hierarchy' are selected. the
#'                  hierarchy variables themselves cannot receive missing values.
#' @param seed integer. a random seed number for reproducing the result (recommended)
#' @details nesting is defined cumulatively. for example, with
#'          'hierarchy = c("schoolid", "childid")' and 'level = "childid"',
#'          children are identified by the combination of 'schoolid' and
#'          'childid'. this allows child identifiers to be repeated across
#'          schools.
#'
#'          the function first counts the currently observed cells in the
#'          selected variables for each eligible cluster. clusters are then
#'          randomly ordered, and a complete cluster is selected whenever adding
#'          it moves the achieved amount of missingness closer to the target
#'          defined by 'p'. consequently, 'p' represents a target proportion of
#'          added missingness rather than an exact cluster sampling fraction.
#'
#'          when several variables are supplied in 'variables', the same sampled
#'          clusters receive missing values for all selected variables. to create
#'          independent hierarchical missingness patterns for different variables,
#'          call the function separately for each variable or variable set.
#' @return data.frame. the returned data frame contains the added missing values.
#'         the target proportion, achieved proportion, selected hierarchy level,
#'         selected variables, and sampled clusters are stored as attributes
#'         'mlim.na.target.p', 'mlim.na.achieved.p', 'mlim.na.level',
#'         'mlim.na.variables', and 'mlim.na.selected.clusters', respectively.
#'         'mlim.na.selected.clusters' is a data frame containing the hierarchy
#'         values that identify each sampled cluster.
#' @author E. F. Haghish
#' @examples
#' \dontrun{
#' if (requireNamespace("mlmRev", quietly = TRUE)) {
#'
#'   data("egsingle", package = "mlmRev")
#'
#'   dat <- egsingle
#'
#'   # Add child-level missingness to female for approximately 20 percent
#'   # of the currently observed values
#'   dat_child <- mlim.na.multilevel(
#'     dat,
#'     p = 0.20,
#'     hierarchy = c("schoolid", "childid"),
#'     level = "childid",
#'     variables = "female",
#'     seed = 2022
#'   )
#'
#'   # Add school-level missingness to black and hispanic. The same schools
#'   # will have both variables set to NA.
#'   dat_school <- mlim.na.multilevel(
#'     dat,
#'     p = 0.20,
#'     hierarchy = c("schoolid", "childid"),
#'     level = "schoolid",
#'     variables = c("black", "hispanic"),
#'     seed = 2022
#'   )
#'
#'   # Inspect the achieved proportion of added missingness
#'   attr(dat_school, "mlim.na.achieved.p")
#' }
#' }
#' @export

mlim.na.multilevel <- function(x,
                               p = 0.1,
                               hierarchy,
                               level,
                               variables = NULL,
                               seed = NULL) {

  # Syntax processing
  # ------------------------------------------------------------
  if (!is.data.frame(x) || any(class(x) %in% c("tbl", "tbl_df", "data.table"))) {
    stop("'x' must be strictly a data.frame.")
  }

  if (length(p) != 1 || !is.numeric(p) || is.na(p) || p < 0 || p > 1) {
    stop("'p' should be a single numeric value between 0 and 1.")
  }

  if (!is.character(hierarchy) || length(hierarchy) < 1) {
    stop("'hierarchy' must be a non-empty character vector.")
  }

  if (anyDuplicated(hierarchy)) {
    stop("'hierarchy' cannot contain duplicated variable names.")
  }

  if (!all(hierarchy %in% names(x))) {
    missing_vars <- setdiff(hierarchy, names(x))
    stop(
      "Hierarchy variables not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  if (anyNA(x[hierarchy])) {
    stop("Hierarchy variables cannot contain missing values.")
  }

  if (!is.character(level) || length(level) != 1 || is.na(level)) {
    stop("'level' must be a single character string.")
  }

  if (!level %in% hierarchy) {
    stop("'level' must name one variable included in 'hierarchy'.")
  }

  if (is.null(variables)) {
    variables <- setdiff(names(x), hierarchy)
  }

  if (!is.character(variables) || length(variables) < 1) {
    stop("'variables' must be a non-empty character vector or NULL.")
  }

  if (anyDuplicated(variables)) {
    variables <- unique(variables)
  }

  if (!all(variables %in% names(x))) {
    missing_vars <- setdiff(variables, names(x))
    stop(
      "Variables not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  if (any(variables %in% hierarchy)) {
    stop("Variables included in 'hierarchy' cannot receive missing values.")
  }

  if (!is.null(seed)) {
    if (length(seed) != 1 || !is.numeric(seed) || is.na(seed)) {
      stop("'seed' must be a single numeric value or NULL.")
    }
    set.seed(seed)
  }

  # Define the requested hierarchical clusters
  # ------------------------------------------------------------
  level_index <- match(level, hierarchy)
  grouping_vars <- hierarchy[seq_len(level_index)]

  group <- do.call(
    interaction,
    c(
      x[grouping_vars],
      list(
        drop = TRUE,
        lex.order = TRUE
      )
    )
  )

  group_chr <- as.character(group)
  cluster_ids <- unique(group_chr)

  # Count currently observed target cells in each cluster
  # ------------------------------------------------------------
  observed_cells <- rowSums(!is.na(x[variables]))

  cluster_observed <- vapply(
    cluster_ids,
    function(id) sum(observed_cells[group_chr == id]),
    numeric(1)
  )

  # Clusters with no observed target cells cannot add missingness
  eligible <- cluster_observed > 0
  cluster_ids <- cluster_ids[eligible]
  cluster_observed <- cluster_observed[eligible]

  total_observed <- sum(cluster_observed)

  # Return unchanged data if there is nothing to remove or p = 0
  # ------------------------------------------------------------
  if (total_observed == 0 || p == 0) {
    out <- x

    attr(out, "mlim.na.target.p") <- p
    attr(out, "mlim.na.achieved.p") <- 0
    attr(out, "mlim.na.hierarchy") <- hierarchy
    attr(out, "mlim.na.level") <- level
    attr(out, "mlim.na.variables") <- variables
    attr(out, "mlim.na.selected.clusters") <-
      x[FALSE, grouping_vars, drop = FALSE]

    return(out)
  }

  # Randomly order clusters and retain a cluster when adding it moves
  # the achieved amount of missingness closer to the requested target
  # ------------------------------------------------------------
  random_order <- sample(seq_along(cluster_ids))

  target_missing <- p * total_observed
  current_missing <- 0
  selected <- rep(FALSE, length(cluster_ids))

  for (i in random_order) {

    candidate_missing <- current_missing + cluster_observed[i]

    if (abs(candidate_missing - target_missing) <
        abs(current_missing - target_missing)) {

      selected[i] <- TRUE
      current_missing <- candidate_missing
    }
  }

  selected_clusters <- cluster_ids[selected]

  # Add missing values to every selected variable for all rows
  # belonging to the sampled clusters
  # ------------------------------------------------------------
  out <- x

  selected_rows <- group_chr %in% selected_clusters

  if (any(selected_rows)) {
    for (variable in variables) {
      out[selected_rows, variable] <- NA
    }
  }

  added_missing <- sum(
    !is.na(x[selected_rows, variables, drop = FALSE]) &
      is.na(out[selected_rows, variables, drop = FALSE])
  )

  achieved_p <- added_missing / total_observed

  # Store useful information about the generated missingness
  # ------------------------------------------------------------
  attr(out, "mlim.na.target.p") <- p
  attr(out, "mlim.na.achieved.p") <- achieved_p
  attr(out, "mlim.na.hierarchy") <- hierarchy
  attr(out, "mlim.na.level") <- level
  attr(out, "mlim.na.variables") <- variables
  selected_cluster_data <- unique(
    x[selected_rows, grouping_vars, drop = FALSE]
  )
  rownames(selected_cluster_data) <- NULL

  attr(out, "mlim.na.selected.clusters") <- selected_cluster_data

  return(out)
}
