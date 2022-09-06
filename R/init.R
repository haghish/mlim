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
  keepTrying <- TRUE
  connection <- NULL
  test       <- 1
  while (keepTrying) {
    tryCatch(connection <- h2o::h2o.init(nthreads = nthreads,
                                         #name = "mlim_connection",
                                         min_mem_size = min_mem_size,
                                         max_mem_size = max_mem_size,
                                         ignore_config = ignore_config,
                                         insecure = TRUE),
             error = function(cond) {
               #cat("connection to JAVA server failed...\n");
               return()})
    if (!is.null(connection)) {
      keepTrying <- FALSE
    }
    else {
      test <- test + 1
      cat("Java server could not be ignitiated. will try again in 3 secs...\n")
      Sys.sleep(3)
    }

    if (test > 10) stop("gave up trying. there is a problem on your system \nthat does not allow h2o server to be started...\n")

  }

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
