#' @title Evaluate imputation error
#' @description Calculates normalized RMSE for numeric
#' variables, mean per-class error for unordered factors,
#' and normalized rank error for ordered factors.
#' @param imputed Imputed data frame, \code{mlim} object,
#' \code{mlim.mi} object, list of completed data frames,
#' or \code{mids} object.
#' @param incomplete Original incomplete data frame.
#' @param complete Original complete data frame used as
#' the reference.
#' @param transform Optional transformation for numeric
#' variables. Supported values are \code{"standardize"}
#' and \code{"normalize"}. The same transformation,
#' estimated from \code{complete}, is applied to all
#' three datasets.
#' @param varwise Logical. If \code{TRUE}, variable-wise
#' error estimates are returned in addition to overall
#' estimates.
#' @param ignore.missclass Logical. If \code{FALSE},
#' ordinary misclassification error is also returned for
#' unordered factors. The default is \code{TRUE}.
#' @param ignore.rank Logical. If \code{FALSE}, ordered
#' factors are evaluated using normalized rank distance.
#' If \code{TRUE}, they are treated as unordered factors.
#' @return A named numeric vector, or a list when
#' \code{varwise = TRUE}. For multiple imputations,
#' returns a matrix or a list of variable-wise results.
#' @author E. F. Haghish
#' @examples
#' \dontrun{
#' data(iris)
#' irisNA <- mlim.na(
#'   iris, p = 0.1,
#'   stratify = TRUE,
#'   seed = 2022
#' )
#'
#' imp <- mlim(irisNA)
#' mlim.error(imp, irisNA, iris)
#'
#' mlim.error(
#'   imp, irisNA, iris,
#'   varwise = TRUE
#' )
#' }
#' @export

mlim.error <- function(imputed, incomplete, complete,
                       transform = NULL, varwise = FALSE,
                       ignore.missclass = TRUE,
                       ignore.rank = FALSE) {

  if (!is.logical(varwise) ||
      length(varwise) != 1L ||
      is.na(varwise)) {
    stop(
      "'varwise' must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(ignore.missclass) ||
      length(ignore.missclass) != 1L ||
      is.na(ignore.missclass)) {
    stop(
      "'ignore.missclass' must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(ignore.rank) ||
      length(ignore.rank) != 1L ||
      is.na(ignore.rank)) {
    stop(
      "'ignore.rank' must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.null(transform)) {

    if (!is.character(transform) ||
        length(transform) != 1L ||
        is.na(transform)) {
      stop(
        "'transform' must be NULL or character.",
        call. = FALSE
      )
    }

    transform <- tolower(transform)

    if (!transform %in%
        c("standardize", "normalize")) {
      stop(
        paste(
          "'transform' must be 'standardize',",
          "'normalize', or NULL."
        ),
        call. = FALSE
      )
    }
  }

  # Multiple-imputation objects
  # ========================================================
  if (inherits(imputed, "mids")) {

    if (!requireNamespace(
      "mice", quietly = TRUE
    )) {
      stop(
        paste(
          "The 'mice' package is required",
          "to evaluate a 'mids' object."
        ),
        call. = FALSE
      )
    }

    imputed <- mice::complete(
      imputed,
      action = "all"
    )
  }

  if (inherits(imputed, "mlim.mi") ||
      (is.list(imputed) &&
       !is.data.frame(imputed))) {

    if (length(imputed) < 1L) {
      stop(
        "No completed datasets were supplied.",
        call. = FALSE
      )
    }

    results <- vector(
      "list", length(imputed)
    )

    for (i in seq_along(imputed)) {

      results[[i]] <- mlim.error(
        imputed = imputed[[i]],
        incomplete = incomplete,
        complete = complete,
        transform = transform,
        varwise = varwise,
        ignore.missclass = ignore.missclass,
        ignore.rank = ignore.rank
      )
    }

    names(results) <- paste0(
      "imputation_", seq_along(results)
    )

    if (varwise) {
      return(results)
    }

    metric_names <- unique(
      unlist(lapply(results, names))
    )

    mat <- matrix(
      NA_real_,
      nrow = length(results),
      ncol = length(metric_names),
      dimnames = list(
        names(results),
        metric_names
      )
    )

    for (i in seq_along(results)) {
      mat[
        i, names(results[[i]])
      ] <- results[[i]]
    }

    return(mat)
  }

  # Single completed dataset
  # ========================================================
  if (!is.data.frame(imputed)) {
    stop(
      paste(
        "'imputed' must be a data.frame,",
        "'mlim', 'mlim.mi', list, or 'mids'."
      ),
      call. = FALSE
    )
  }

  if (!is.data.frame(incomplete) ||
      !is.data.frame(complete)) {
    stop(
      paste(
        "'incomplete' and 'complete'",
        "must be data.frames."
      ),
      call. = FALSE
    )
  }

  if (anyNA(complete)) {
    stop(
      "'complete' must not contain missing values.",
      call. = FALSE
    )
  }

  if (nrow(imputed) != nrow(incomplete) ||
      nrow(complete) != nrow(incomplete)) {
    stop(
      "All datasets must have the same rows.",
      call. = FALSE
    )
  }

  if (!identical(
    names(imputed), names(incomplete)
  ) ||
  !identical(
    names(complete), names(incomplete)
  )) {
    stop(
      paste(
        "All datasets must have identical",
        "variables and variable order."
      ),
      call. = FALSE
    )
  }

  naCols <- which(
    colSums(is.na(incomplete)) > 0L
  )

  if (length(naCols) < 1L) {
    stop(
      "'incomplete' contains no missing values.",
      call. = FALSE
    )
  }

  imputed <- imputed[
    , naCols, drop = FALSE
  ]
  incomplete <- incomplete[
    , naCols, drop = FALSE
  ]
  complete <- complete[
    , naCols, drop = FALSE
  ]

  classes <- lapply(complete, class)
  types <- vapply(
    classes,
    function(x) x[1],
    character(1)
  )

  types[types == "integer"] <- "numeric"

  if (ignore.rank) {
    types[types == "ordered"] <- "factor"
  }

  supported <- c(
    "numeric", "ordered", "factor",
    "character", "logical"
  )

  if (any(!types %in% supported)) {
    bad <- unique(types[!types %in% supported])
    stop(
      paste(
        "Unsupported variable class:",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  nrmse_error <- NULL
  mpce_error <- NULL
  class_error <- NULL
  rank_error <- NULL

  # Numeric variables
  # ========================================================
  ind <- which(types == "numeric")

  if (length(ind) > 0L) {

    v1 <- imputed[, ind, drop = FALSE]
    v2 <- incomplete[, ind, drop = FALSE]
    v3 <- complete[, ind, drop = FALSE]

    if (!is.null(transform)) {

      for (j in seq_along(v3)) {

        if (transform == "standardize") {

          center <- mean(
            v3[[j]], na.rm = TRUE
          )
          spread <- stats::sd(
            v3[[j]], na.rm = TRUE
          )

          if (is.finite(spread) &&
              spread > 0) {
            v1[[j]] <- (
              v1[[j]] - center
            ) / spread
            v2[[j]] <- (
              v2[[j]] - center
            ) / spread
            v3[[j]] <- (
              v3[[j]] - center
            ) / spread
          }
        }

        if (transform == "normalize") {

          lower <- min(
            v3[[j]], na.rm = TRUE
          )
          upper <- max(
            v3[[j]], na.rm = TRUE
          )
          spread <- upper - lower

          if (is.finite(spread) &&
              spread > 0) {
            v1[[j]] <- (
              v1[[j]] - lower
            ) / spread
            v2[[j]] <- (
              v2[[j]] - lower
            ) / spread
            v3[[j]] <- (
              v3[[j]] - lower
            ) / spread
          }
        }
      }
    }

    nrmse_error <- nrmse(
      v1, v2, v3
    )

    valid <- is.finite(nrmse_error)

    if (!any(valid)) {
      nrmse_error <- NULL
    }
  }

  # Ordered factors
  # ========================================================
  ind <- which(types == "ordered")

  if (length(ind) > 0L &&
      !ignore.rank) {

    rank_error <- missrank(
      imputed[, ind, drop = FALSE],
      incomplete[, ind, drop = FALSE],
      complete[, ind, drop = FALSE]
    )

    valid <- is.finite(rank_error)

    if (!any(valid)) {
      rank_error <- NULL
    }
  }

  # Unordered categorical variables
  # ========================================================
  ind <- which(
    types %in% c(
      "factor", "character", "logical"
    )
  )

  if (length(ind) > 0L) {

    mpce_error <- numeric(
      length(ind)
    )
    names(mpce_error) <- names(
      complete
    )[ind]

    if (!ignore.missclass) {
      class_error <- numeric(
        length(ind)
      )
      names(class_error) <- names(
        complete
      )[ind]
    }

    for (j in seq_along(ind)) {

      col <- ind[j]

      mpce_error[j] <-
        mean_per_class_error(
          imputed[
            , col, drop = FALSE
          ],
          incomplete[
            , col, drop = FALSE
          ],
          complete[
            , col, drop = FALSE
          ],
          mean = TRUE
        )

      if (!ignore.missclass) {
        class_error[j] <- missclass(
          imputed[
            , col, drop = FALSE
          ],
          incomplete[
            , col, drop = FALSE
          ],
          complete[
            , col, drop = FALSE
          ]
        )[1]
      }
    }

    mpce_error[
      !is.finite(mpce_error)
    ] <- NA_real_

    if (!ignore.missclass) {
      class_error[
        !is.finite(class_error)
      ] <- NA_real_
    }
  }

  # Overall error estimates
  # ========================================================
  err <- numeric()

  if (!is.null(nrmse_error)) {
    valid <- is.finite(nrmse_error)

    if (any(valid)) {
      err["nrmse"] <- mean(
        nrmse_error[valid]
      )
    }
  }

  if (!is.null(mpce_error)) {
    valid <- is.finite(mpce_error)

    if (any(valid)) {
      err["mpce"] <- mean(
        mpce_error[valid]
      )
    }
  }

  if (!ignore.missclass &&
      !is.null(class_error)) {
    valid <- is.finite(class_error)

    if (any(valid)) {
      err["missclass"] <- mean(
        class_error[valid]
      )
    }
  }

  if (!ignore.rank &&
      !is.null(rank_error)) {
    valid <- is.finite(rank_error)

    if (any(valid)) {
      err["missrank"] <- mean(
        rank_error[valid]
      )
    }
  }

  if (!varwise) {
    return(err)
  }

  all_error <- c(
    nrmse_error,
    mpce_error,
    if (!ignore.missclass) {
      class_error
    },
    if (!ignore.rank) {
      rank_error
    }
  )

  return(list(
    error = err,
    nrmse = nrmse_error,
    mpce = mpce_error,
    missclass = class_error,
    missrank = rank_error,
    all = all_error
  ))
}
