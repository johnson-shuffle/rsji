# median income - pareto / linear interpolation

inc_int <- function(inc_cols, type = 'pareto') {
  
  x <- t(inc_cols) ; stopifnot(x[1] != 0)
  
  # parameters
  N  <- x[1]
  bn <- match(T, N / 2 < cumsum(x[-1]))
  lo <- c(0, seq(1E4, 5E4, by = 5E3), 6E4, seq(75E3, 15E4, by = 25E3), 2E5)
  hi <- c(1E4, (lo - 1)[3:length(lo)], Inf)
  A1 <- lo[bn]
  A2 <- hi[bn]
  N1 <- sum(x[bn:16])
  N2 <- sum(x[(bn + 1):16])
  
  # fill in if empty income bins
  i <- 1
  while (N1 == N2) { N1 <- sum(x[(bn - i):16]) ; i <- i + 1 }
  
  # interpolation
  M1 <- A1 * exp(log(0.5 * N / N1) / log(N2 / N1) * log(A2 / A1))
  M2 <- A1 + (0.5 * N - N1) / (N2 - N1) * (A2 - A1)
  
  ifelse(type == 'pareto', M1, M2)

}
