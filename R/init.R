#' @title init
#' @description Initiates the H2O server.
#' @author E. F. Haghish
#' @return H2O connection object.
#' @keywords Internal
#' @noRd

init <- function(nthreads,
                 min_mem_size,
                 max_mem_size,
                 ignore_config = TRUE,
                 java = NULL,
                 debug = FALSE,
                 port = 54321) {

  if (!is.null(java)) {
    Sys.setenv(JAVA_HOME = java)
  }

  connection <- NULL

  for (attempt in seq_len(10L)) {

    connection <- tryCatch(
      h2o::h2o.init(
        nthreads = nthreads,
        min_mem_size = min_mem_size,
        max_mem_size = max_mem_size,
        ignore_config = ignore_config,
        port = port,
        insecure = TRUE,
        https = FALSE,
        log_level = if (debug) "DEBUG" else "FATA",
        bind_to_localhost = TRUE
      ),
      error = function(cond) NULL
    )

    if (!is.null(connection)) {
      return(connection)
    }

    if (attempt < 10L) {
      message(
        "The H2O server could not be initiated. ",
        "Retrying in 3 seconds...\n"
      )
      Sys.sleep(3)
    }
  }

  stop(
    "The attempt to start the H2O server was unsuccessful ",
    "due to an issue within your system."
  )
}
