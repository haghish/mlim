#' @title server.check
#' @description safely examines the connection status with h2o server
#' @param connection H2O connection object.
#' @author E. F. Haghish
#' @return logical. if TRUE, proceed with the analysis
#' @keywords Internal
#' @noRd

server.check <- function(connection) {

  up      <- FALSE
  healthy <- FALSE

  # check that the cluster is up
  # ============================================================
  for (i in 1:5) {
    if (!up) tryCatch(
      up <- h2o::h2o.clusterIsUp(connection),
      error = function(cond) {
        message("trying to connect to JAVA server...\n")
        return(NULL)
      }
    )
    if (!up) Sys.sleep(0.25)
  }

  if (!up) return(FALSE)

  # make sure the cluster is healthy
  # ============================================================
  for (i in 1:5) {
    if (!healthy) tryCatch({
      status <- NULL
      capture.output(status <- h2o::h2o.clusterStatus())
      healthy <- nrow(status) > 0L && isTRUE(all(status$healthy))
    },
    error = function(cond) {
      message("trying to connect to JAVA server...\n")
      return(NULL)
    })
    if (!healthy) Sys.sleep(0.25)
  }

  return(isTRUE(healthy))
}
