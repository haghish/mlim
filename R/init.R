
#' @title extractMetrics
#' @description extracts performance metrics from cross-validation
#' @author E. F. Haghish
#' @return connection object
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

init <- function(nthreads, min_mem_size, max_mem_size, ignore_config = TRUE,
                 md.log) {


  # Run H2O on the statistics server
  # ============================================================
  #try(h2o.shutdown(FALSE), silent = TRUE)
  connection <- h2o::h2o.init(nthreads = nthreads,
                              min_mem_size = min_mem_size,
                              max_mem_size = max_mem_size,
                              ignore_config = ignore_config)

  #if (!is.null(md.log)) {
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