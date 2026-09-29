#' Stop an H2O server running on a local port
#'
#' Checks whether a local TCP port is open. If it is open, the function
#' verifies that the service is H2O through the /3/Cloud REST endpoint.
#' An H2O server is shut down through POST /3/Shutdown, and the function
#' waits until the port is released before returning.
#'
#' @param port Integer H2O REST API port.
#' @param host Character host name or IP address. Defaults to 127.0.0.1.
#' @param timeout Numeric connection/request timeout in seconds.
#' @param wait Numeric maximum number of seconds to wait for shutdown.
#' @return Invisibly TRUE if an H2O server was stopped, FALSE if the
#'   requested port was already free.
#' @keywords Internal
#' @noRd
stopH2o <- function(port = 54321,
                          host = "127.0.0.1",
                          timeout = 1,
                          wait = 10) {

  port <- as.integer(port)[1]

  if (!is.finite(port) || port < 1L || port > 65535L) {
    stop("'port' must be an integer between 1 and 65535.", call. = FALSE)
  }

  if (!is.numeric(timeout) || length(timeout) != 1L ||
      !is.finite(timeout) || timeout <= 0) {
    stop("'timeout' must be a positive number.", call. = FALSE)
  }

  if (!is.numeric(wait) || length(wait) != 1L ||
      !is.finite(wait) || wait <= 0) {
    stop("'wait' must be a positive number.", call. = FALSE)
  }

  portOpen <- function() {
    con <- tryCatch(
      socketConnection(
        host = host,
        port = port,
        open = "r+b",
        blocking = TRUE,
        timeout = timeout
      ),
      error = function(e) NULL
    )

    if (is.null(con)) return(FALSE)

    try(close(con), silent = TRUE)
    TRUE
  }

  # Nothing is listening on this port.
  if (!portOpen()) {
    return(invisible(FALSE))
  }

  base_url <- paste0("http://", host, ":", port)

  get_handle <- curl::new_handle(
    connecttimeout = timeout,
    timeout = timeout,
    noproxy = "*"
  )

  cloud <- tryCatch(
    curl::curl_fetch_memory(
      paste0(base_url, "/3/Cloud"),
      handle = get_handle
    ),
    error = function(e) e
  )

  if (inherits(cloud, "error")) {
    stop(
      paste0(
        "Port ", port,
        " is in use, but the service could not be verified as H2O: ",
        conditionMessage(cloud)
      ),
      call. = FALSE
    )
  }

  cloud_text <- rawToChar(cloud$content)

  is_h2o <- cloud$status_code >= 200L &&
    cloud$status_code < 300L &&
    grepl('"cloud_name"|"cloud_size"|"nodes"', cloud_text)

  if (!is_h2o) {
    stop(
      paste0(
        "Port ", port,
        " is already in use by a service that is not recognized as H2O."
      ),
      call. = FALSE
    )
  }

  post_handle <- curl::new_handle(
    customrequest = "POST",
    connecttimeout = timeout,
    timeout = timeout,
    noproxy = "*"
  )

  shutdown <- tryCatch(
    curl::curl_fetch_memory(
      paste0(base_url, "/3/Shutdown"),
      handle = post_handle
    ),
    error = function(e) e
  )

  if (inherits(shutdown, "error")) {
    stop(
      paste0(
        "The H2O server on port ", port,
        " was found but could not be shut down: ",
        conditionMessage(shutdown)
      ),
      call. = FALSE
    )
  }

  if (shutdown$status_code < 200L || shutdown$status_code >= 300L) {
    stop(
      paste0(
        "H2O shutdown on port ", port,
        " returned HTTP status ", shutdown$status_code, "."
      ),
      call. = FALSE
    )
  }

  deadline <- Sys.time() + wait

  while (portOpen()) {
    if (Sys.time() >= deadline) {
      stop(
        paste0(
          "H2O was asked to shut down on port ", port,
          ", but the port was still open after ", wait, " seconds."
        ),
        call. = FALSE
      )
    }

    Sys.sleep(0.25)
  }

  invisible(TRUE)
}
