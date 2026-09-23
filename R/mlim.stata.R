#' Prepare Multiple Imputation Data for Stata
#'
#' Converts an object of class \code{mlim.mi} to a data frame that can be
#' imported into Stata as multiple imputation data. Currently, only Stata's
#' \code{flong} format is supported.
#'
#' @param mlim An object of class \code{mlim.mi} containing the multiple imputation
#'   datasets.
#' @param df A data frame containing the original unimputed dataset used for
#'   imputation.
#' @param format Character string specifying the Stata multiple imputation
#'   format. Currently, only \code{"flong"} is supported.
#' @param filename Optional character string specifying the file name or path
#'   for saving the prepared dataset as a Stata \code{.dta} file. 
#'
#' @details
#' For the \code{flong} format, the original unimputed dataset is assigned
#' \code{m = 0}, and the imputed datasets are assigned \code{m = 1, ..., M}.
#' An \code{id} variable is added to identify the same observation across the
#' original and imputed datasets. All datasets are then apprended by rows.
#'
#' @return A data frame containing the original and imputed datasets in Stata
#'   \code{flong} format.
#'
#' @examples
#' \dontrun{
#' imp <- mlim(data = df, m = 5)
#'
#' stata.data <- mlim.stata(
#'   mlim = imp,
#'   df = df,
#'   format = "flong"
#' )
#'
#' mlim.stata(
#'   mlim = imp,
#'   df = df,
#'   filename = "imputed_data.dta"
#' )
#' }
#'
#' @export

mlim.stata <- function(mlim, df, format = "flong", filename = NULL) {
  
  format <- tolower(format)
  if (!format %in% "flong") stop("currently, only 'flong' format is supported!")
  
  if (!inherits(mlim, "mlim.mi")) stop("'mlim' must be of class mlim.mi")
  if (!is.data.frame(df)) stop("'df' must be the original unimputed data.frame.")
  
  if (!is.null(filename) && nzchar(filename)) {
    if (!requireNamespace("readstata13", quietly = TRUE)) {
      stop("The 'readstata13' package is required to save a Stata file.")
    }
  }
  
  # Avoid overwriting existing variables called "m" and "id"
  if (any(c("m", "id") %in% names(df))) {
    stop("The variables 'm' and 'id' already exist in the dataset.")
  }
  
  # Add m = 0 original dataset
  n <- nrow(df)
  df$m  <- 0L
  df$id <- seq_len(n)
  
  # add "m" and "id" to each imputed dataset and rbind them into flong format
  imputations <- lapply(seq_along(mlim), function(i) {
    x <- mlim[[i]]
    x$m  <- as.integer(i)
    x$id <- seq_len(n)
    x
  })
  
  stata.data <- do.call(rbind, c(list(df), imputations))
  rownames(stata.data) <- NULL
  
  # Save Stata file if requested
  # ============================================================
  if (!is.null(filename) && nzchar(filename)) {
    if (!grepl("\\.dta$", filename, ignore.case = TRUE)) {
      filename <- paste0(filename, ".dta")
    }
    
    readstata13::save.dta13(
      data = stata.data,
      file = filename,
      convert.factors = TRUE,
      add.rownames = FALSE
    )
  }
  
  return(stata.data)
}