rpois_rnorm <- function(N, x, y) {
  replicate(N, rpois(1, exp(x + rnorm(1, 0, y))))
}
