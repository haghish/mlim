#' @title server.check
#' @description safely examines the connection status with h2o server
#' @importFrom h2o h2o.clusterIsUp
#' @author E. F. Haghish
#' @return logical. if TRUE, proceed with the analysis
#' @keywords Internal
#' @noRd

server.check <- function() {

  up      <- FALSE
  healthy <- FALSE

  # check that the cluster is up
  # ============================================================
  for (i in 1:5) {
    if (!up) tryCatch(up <- h2o.clusterIsUp(),
                            error = function(cond){
                              message("trying to connect to JAVA server...\n");
                              return(NULL)})
    if (!up) Sys.sleep(3)
  }
  if (!up) stop("h2o server is down... perhaps heavy RAM consumption crashed the JAVA server?\nNOTE: save your imputation with the 'save' argument to continue \n      from where the imputation crashed")

  # make sure the cluster is healthy
  # ============================================================
  for (i in 1:5) {
    if (!healthy) tryCatch(healthy <- h2o.clusterStatus()$healthy,
                                      error = function(cond){
                                        message("trying to connect to JAVA server...\n");
                                        return(NULL)})
    if (!healthy) Sys.sleep(3)
  }
  if (!healthy) stop("h2o server is down... perhaps heavy RAM consumption crashed the JAVA server?\nNOTE: save your imputation with the 'save' argument to continue \n      from where the imputation crashed")


  return(healthy)
}

