#' @title Summarize mlim imputation error
#' @description Summarizes the best accepted
#' cross-validation RMSE for each imputed variable.
#' @param data An object returned by \code{mlim()}.
#' @return For a single imputation, a data frame containing
#' variable names and RMSE values. For multiple imputation,
#' a data frame containing one RMSE column per imputation.
#' @examples
#' \dontrun{
#' data(iris)
#'
#' irisNA <- iris
#' irisNA$Species <- mlim.na(
#'   irisNA$Species,
#'   p = 0.1,
#'   stratify = TRUE,
#'   seed = 2022
#' )
#'
#' imp <- mlim(irisNA)
#' mlim.summarize(imp)
#' }
#' @author E. F. Haghish
#' @export

mlim.summarize <- function(data) {

  if (inherits(data, "mlim")) {

    metrics <- attr(data, "metrics")

    if (is.null(metrics)) {
      stop(
        "The 'mlim' object does not contain metrics.",
        call. = FALSE
      )
    }

    if (!all(c("variable", "RMSE") %in% names(metrics))) {
      stop(
        paste(
          "The stored metrics do not contain",
          "variable and RMSE."
        ),
        call. = FALSE
      )
    }

    VARS <- colnames(data)[
      colnames(data) %in% metrics$variable
    ]

    results <- data.frame(
      variable = VARS,
      rmse = NA_real_,
      stringsAsFactors = FALSE
    )

    for (i in seq_along(VARS)) {

      index <- metrics$variable == VARS[i]

      results$rmse[i] <- round(
        min(metrics[index, "RMSE"], na.rm = TRUE),
        6
      )
    }

    return(results)
  }

  if (inherits(data, "mlim.mi")) {

    if (length(data) < 1L) {
      stop(
        "The 'mlim.mi' object contains no imputations.",
        call. = FALSE
      )
    }

    results <- mlim.summarize(data[[1]])

    colnames(results)[2] <- "rmse_1"

    if (length(data) > 1L) {

      for (i in 2:length(data)) {

        current <- mlim.summarize(data[[i]])

        results[[paste0("rmse_", i)]] <-
          current$rmse[
            match(results$variable, current$variable)
          ]
      }
    }

    return(results)
  }

  stop(
    paste(
      "'data' must be an object of class",
      "'mlim' or 'mlim.mi'."
    ),
    call. = FALSE
  )
}
