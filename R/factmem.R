#' @title factmem
#' @description memorizes factors' levels
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd
factmem <- function(df) {
  mem <- list()
  for (i in 1:ncol(df)) {
    lev <- levels(df[,i])
    len <- 1:length(lev)
    nam <- colnames(df)[i]
    mem[[i]] <- list(names = nam, length = len, level=lev)
  }

  return(mem)
}

