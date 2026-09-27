#' @title convert multiple imputations to a mids object
#' @description converts multiply imputed datasets to a
#' \code{mids} object for analysis with \code{mice}.
#' @importFrom mice as.mids
#' @param mlim An object of class \code{"mlim.mi"} returned
#' by \code{mlim()}, or a compatible MI object.
#' @param incomplete The original incomplete data frame.
#' @return An object of class \code{"mids"}.
#' @details The original data are stored as imputation 0
#' and completed datasets as imputations 1, ..., m.
#' The function verifies dataset dimensions, variable order,
#' observed values, and completion of originally missing
#' values before conversion.
#' @author E. F. Haghish
#' @examples
#' \dontrun{
#' data(iris)
#' irisNA <- mlim.na(iris, p = 0.1, seed = 2022)
#' imp <- mlim(
#'   irisNA, m = 5, tuning_time = 180, seed = 2022
#' )
#' mids <- mlim.mids(imp, irisNA)
#' fit <- with(
#'   mids,
#'   lm(Sepal.Length ~ Sepal.Width + Petal.Length)
#' )
#' summary(mice::pool(fit))
#' }
#' @export

mlim.mids <- function(mlim, incomplete) {

  valid_classes <- c(
    "mlim.mi", "MIMCA", "MIFAMD", "MIPCA"
  )

  if (!any(valid_classes %in% class(mlim))) {
    stop(
      paste(
        "'mlim' must have class 'mlim.mi',",
        "'MIMCA', 'MIFAMD', or 'MIPCA'."
      ),
      call. = FALSE
    )
  }

  if (!is.data.frame(incomplete)) {
    stop("'incomplete' must be a data.frame.",
         call. = FALSE)
  }

  if (length(mlim) < 1L) {
    stop("'mlim' contains no imputations.",
         call. = FALSE)
  }

  n <- nrow(incomplete)

  for (i in seq_along(mlim)) {

    current <- mlim[[i]]

    if (!is.data.frame(current)) {
      stop(
        paste("Imputation", i, "is not a data.frame."),
        call. = FALSE
      )
    }

    if (nrow(current) != n) {
      stop(
        paste("Imputation", i, "has different rows."),
        call. = FALSE
      )
    }

    if (!identical(names(current), names(incomplete))) {
      stop(
        paste(
          "Imputation", i,
          "has different variables or variable order."
        ),
        call. = FALSE
      )
    }

    for (j in seq_along(incomplete)) {

      observed <- !is.na(incomplete[[j]])
      original <- incomplete[[j]][observed]
      completed <- current[[j]][observed]

      if (is.factor(original) || is.factor(completed)) {
        same <- identical(
          as.character(original),
          as.character(completed)
        )
      }
      else {
        same <- isTRUE(
          all.equal(
            original, completed,
            check.attributes = FALSE
          )
        )
      }

      if (!same) {
        stop(
          paste(
            "Imputation", i, "changes observed values in",
            paste0("'", names(incomplete)[j], "'.")
          ),
          call. = FALSE
        )
      }

      missing <- !observed
      if (any(missing) &&
          anyNA(current[[j]][missing])) {
        stop(
          paste(
            "Imputation", i,
            "has missing imputed values in",
            paste0("'", names(incomplete)[j], "'.")
          ),
          call. = FALSE
        )
      }
    }
  }

  longformat <- rbind(
    incomplete,
    do.call(rbind, mlim)
  )

  imp_name <- ".mlim_imp"
  id_name <- ".mlim_id"

  while (imp_name %in% names(longformat)) {
    imp_name <- paste0(imp_name, "_")
  }

  while (id_name %in% names(longformat)) {
    id_name <- paste0(id_name, "_")
  }

  m <- length(mlim)
  longformat[[imp_name]] <- rep(
    seq.int(0L, m), each = n
  )
  longformat[[id_name]] <- rep(
    seq_len(n), times = m + 1L
  )

  rownames(longformat) <- NULL

  mids <- as.mids(
    longformat,
    .imp = imp_name,
    .id = id_name
  )

  return(mids)
}
