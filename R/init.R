#' @title extractMetrics
#' @description extracts performance metrics from cross-validation
#' @author E. F. Haghish
#' @return connection object
#' @keywords Internal
#' @noRd

init <- function(nthreads, min_mem_size, max_mem_size, ignore_config = TRUE,
                 java = NULL, report) {

  if (!is.null(java)) {
    Sys.setenv(JAVA_HOME = java)
  }
  # Run H2O on the statistics server
  # ============================================================
  #try(h2o.shutdown(FALSE), silent = TRUE)
  connection <- h2o::h2o.init(nthreads = nthreads,
                              name = "mlim_connection",
                              min_mem_size = min_mem_size,
                              max_mem_size = max_mem_size,
                              ignore_config = ignore_config,
                              insecure = TRUE)

  #if (!is.null(report)) {
  #  if (!is.null(connection)) {
  #    if (attributes(connection)$ip == "localhost") {
  #      md.log("h2o.cluster was initialized", section="subsection")
  #      md.log(paste("IP:",attributes(connection)$ip,
  #                   "   port:",attributes(connection)$port))
  #    }
  #  }
  #}

  return(connection)
}
