

defaultCV <- function(n, min = 10) {
  cv <- NULL
  v <- n / min
  if (v >= 150) cv <- 15
}
