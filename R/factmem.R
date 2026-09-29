#' @title factmem
#' @description memorizes factors' levels and ordinal support
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

factmem <- function(df) {
  mem <- list()

  for (i in seq_len(ncol(df))) {
    lev <- levels(df[[i]])
    nam <- colnames(df)[i]
    mem[[i]] <- list(names = nam, support = seq_along(lev), level = lev)
  }

  return(mem)
}
