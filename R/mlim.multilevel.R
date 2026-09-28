#' @title Prepare Multilevel Data for Imputation
#'
#' @description
#' Prepares hierarchical data for multilevel imputation by adding
#' cluster-level summary variables at each level of the hierarchy.
#'
#' @param data A data frame containing the variables to be imputed and the
#'   variables identifying the hierarchical structure.
#'
#' @param hierarchy A character vector specifying the clustering variables
#'   from the highest to the lowest level. For example,
#'   `hierarchy = c("city", "school", "classroom", "student")`
#'   specifies students nested within classrooms, classrooms nested within
#'   schools, and schools nested within cities.
#'
#' @param variables An optional character vector specifying the variables for
#'   which cluster-level summaries should be generated. By default, all
#'   variables not included in `hierarchy` are used.
#'
#' @param leave_one_out Logical. If `TRUE`, the observation's own value is
#'   excluded when calculating its cluster-level summary. This reduces
#'   information leakage when the summaries are used as predictors during
#'   imputation. The default is `TRUE`.
#'
#' @param add_cluster_size Logical. If `TRUE`, the number of observations in
#'   each cluster is added as an additional variable. The default is `TRUE`.
#'
#' @details
#' The function creates cluster-level summaries at each level of the supplied
#' hierarchy. Continuous and ordinal variables are summarized using cluster
#' means. Binary variables are summarized using proportions. For nominal
#' variables, cluster-level proportions are generated for each non-reference
#' category.
#'
#' Nesting is defined cumulatively. For example, if
#' `hierarchy = c("city", "school", "classroom")`, summaries are generated
#' within city, within school nested in city, and within classroom nested in
#' school and city.
#'
#' A hierarchy level for which every cluster contains only one observation is
#' ignored because no within-cluster information can be calculated.
#'
#' @return
#' A data frame containing the original variables together with the generated
#' cluster-level variables.
#'
#' @examples
#' if (requireNamespace("mlmRev", quietly = TRUE)) {
#'
#'   data("egsingle", package = "mlmRev")
#'
#'   dat <- egsingle
#'
#'   # Add missing values for illustration
#'   set.seed(123)
#'   dat$math[sample(seq_len(nrow(dat)), 500)] <- NA
#'
#'   # Repeated observations are nested within students,
#'   # and students are nested within schools
#'   dat_ml <- mlim.multilevel.R(
#'     data = dat,
#'     hierarchy = c("schoolid", "childid"),
#'     variables = c(
#'       "math",
#'       "female",
#'       "black",
#'       "hispanic"
#'     )
#'   )
#'
#'   head(dat_ml)
#' }
#'
#' @author E. F. Haghish
#' @keywords internal
#' @noRd

mlim.multilevel.R <- function(data,
                              hierarchy,
                              variables = NULL,
                              leave_one_out = TRUE,
                              add_cluster_size = TRUE) {

  # Check input
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  if (!is.character(hierarchy) || length(hierarchy) < 1) {
    stop("'hierarchy' must be a character vector.")
  }

  if (!all(hierarchy %in% names(data))) {
    missing_vars <- setdiff(hierarchy, names(data))
    stop(
      "Hierarchy variables not found in data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  if (anyNA(data[hierarchy])) {
    stop("Hierarchy variables cannot contain missing values.")
  }

  # Variables for which cluster summaries are created
  if (is.null(variables)) {
    variables <- setdiff(names(data), hierarchy)
  }

  if (!all(variables %in% names(data))) {
    stop("Some variables specified in 'variables' are not in the data.")
  }

  out <- data

  # Helper: numeric cluster mean
  cluster_mean <- function(x, group, loo = TRUE) {

    observed <- !is.na(x)

    group_sum <- ave(
      ifelse(observed, x, 0),
      group,
      FUN = sum
    )

    group_n <- ave(
      as.integer(observed),
      group,
      FUN = sum
    )

    if (loo) {

      numerator <- group_sum -
        ifelse(observed, x, 0)

      denominator <- group_n -
        as.integer(observed)

    } else {

      numerator <- group_sum
      denominator <- group_n
    }

    result <- numerator / denominator

    result[denominator == 0] <- NA_real_

    result
  }


  # Helper: cluster proportion
  cluster_prop <- function(x, level, group, loo = TRUE) {

    observed <- !is.na(x)

    indicator <- ifelse(
      observed,
      as.integer(x == level),
      0
    )

    group_sum <- ave(
      indicator,
      group,
      FUN = sum
    )

    group_n <- ave(
      as.integer(observed),
      group,
      FUN = sum
    )

    if (loo) {

      numerator <- group_sum -
        ifelse(observed, as.integer(x == level), 0)

      denominator <- group_n -
        as.integer(observed)

    } else {

      numerator <- group_sum
      denominator <- group_n
    }

    result <- numerator / denominator

    result[denominator == 0] <- NA_real_

    result
  }


  # Work through each level of the hierarchy
  for (level_index in seq_along(hierarchy)) {

    grouping_vars <- hierarchy[seq_len(level_index)]

    # Cumulative nesting:
    # city
    # city + school
    # city + school + classroom
    group <- do.call(
      interaction,
      c(
        data[grouping_vars],
        list(
          drop = TRUE,
          lex.order = TRUE
        )
      )
    )

    level_name <- paste(
      grouping_vars,
      collapse = "_"
    )

    cluster_n <- ave(
      rep(1L, nrow(data)),
      group,
      FUN = length
    )

    # Skip individual-level IDs
    if (all(cluster_n == 1)) {
      next
    }

    # Add cluster size
    if (add_cluster_size) {

      new_name <- paste0(
        "_",
        level_name,
        "_n"
      )

      out[[new_name]] <- cluster_n
    }


    # Add summaries for each substantive variable
    for (variable in variables) {

      x <- data[[variable]]

      # ------------------------------------------
      # Continuous variable
      # ------------------------------------------

      if (is.numeric(x) && !is.factor(x)) {

        new_name <- paste0(
          "_",
          level_name,
          "_",
          variable,
          "_mean"
        )

        out[[new_name]] <- cluster_mean(
          x,
          group,
          loo = leave_one_out
        )

      }

      # ------------------------------------------
      # Ordinal variable
      # ------------------------------------------

      else if (is.ordered(x)) {

        x_num <- as.numeric(x)

        new_name <- paste0(
          "_",
          level_name,
          "_",
          variable,
          "_mean"
        )

        out[[new_name]] <- cluster_mean(
          x_num,
          group,
          loo = leave_one_out
        )

      }

      # ------------------------------------------
      # Binary variable
      # ------------------------------------------

      else if (
        is.factor(x) &&
        length(levels(x)) == 2
      ) {

        lev <- levels(x)[2]

        new_name <- paste0(
          "_",
          level_name,
          "_",
          variable,
          "_prop"
        )

        out[[new_name]] <- cluster_prop(
          x,
          lev,
          group,
          loo = leave_one_out
        )

      }

      # ------------------------------------------
      # Nominal variable
      # ------------------------------------------

      else if (
        is.factor(x) ||
        is.character(x)
      ) {

        x <- factor(x)

        levs <- levels(x)

        # First category is reference category
        if (length(levs) > 1) {

          for (lev in levs[-1]) {

            safe_level <- make.names(lev)

            new_name <- paste0(
              "_",
              level_name,
              "_",
              variable,
              "_prop_",
              safe_level
            )

            out[[new_name]] <- cluster_prop(
              x,
              lev,
              group,
              loo = leave_one_out
            )
          }
        }
      }
    }
  }

  attr(out, "mlim.hierarchy") <- hierarchy
  attr(out, "mlim.original.variables") <- names(data)
  attr(out, "mlim.multilevel.variables") <-
    setdiff(names(out), names(data))

  out
}
