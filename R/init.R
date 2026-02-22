#' @title init
#' @description initiates h2o server
#' @author E. F. Haghish
#' @return connection object
#' @keywords Internal
#' @noRd

init <- function(nthreads, min_mem_size, max_mem_size, ignore_config = TRUE,
                 java = NULL, report, debug) {

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
    # h2o.init(jvm_custom_args = c("-help"))
    # h2o.init(jvm_custom_args = c("-session_timeout=100"))
    # bind_to_localhost = FALSE
    # h2o.init(jvm_custom_args=c("-Dsys.ai.h2o.heartbeat.benchmark.enabled=true"))
    tryCatch(connection <- h2o::h2o.init(nthreads = nthreads,
                                         #name = "mlim_connection",
                                         min_mem_size = min_mem_size,
                                         max_mem_size = max_mem_size,
                                         ignore_config = ignore_config,
                                         insecure = TRUE,
                                         https = FALSE,
                                         log_level = if (debug) "DEBUG" else "FATA",
                                         bind_to_localhost = FALSE),
             error = function(cond) {
               #message("connection to JAVA server failed...\n");
               return()})
    if (!is.null(connection)) {
      keepTrying <- FALSE
    }
    else {
      test <- test + 1
      message("The Java server could not be initiated. It will retry in 3 seconds...\n")
      Sys.sleep(3)
    }

    if (test > 10) stop("The attempt to start the H2O server was unsuccessful \ndue to an issue within your system...\n")

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
