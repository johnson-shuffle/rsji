extract_pars <- function(m, pars = NULL, adjust = F, labs = NULL) {
  x <- rstan::extract(fit[[m]]$result, pars = pars, inc_warmup = F)[[1]]
  x %<>% as_tibble()
  if (adjust & !pars %in% c('mu', 'nu')) { x <- x - rowSums(x) / ncol(x) }
  if (adjust & pars == 'mu') {
    y <- rstan::extract(fit[[m]]$result, pars = 'alpha')[[1]]
    y <- rowSums(y) / dim(y)[2]
    z <- rstan::extract(fit[[m]]$result, pars = 'beta')[[1]]
    z <- rowSums(z) / dim(z)[2]
    x <- x + y + z
  }
  if (adjust & pars == 'nu') {
    y <- rstan::extract(fit[[m]]$result, pars = 'delta')[[1]]
    y <- rowSums(y) / dim(y)[2]
    z <- rstan::extract(fit[[m]]$result, pars = 'zeta')[[1]]
    z <- rowSums(z) / dim(z)[2]
    x <- x + y + z
  }
  avg <- apply(x, 2, mean)
  std <- apply(x, 2, sd)
  hpds <- coda::HPDinterval(coda::mcmc(x))
  out  <- tibble(
    label = pars,
    model = m,
    mean  = avg,
    sd = std,
    lower = hpds[, 'lower'],
    upper = hpds[, 'upper']
  )
  if (!is.null(labs)) { out$label <- labs }
  if (is.null(labs) & nrow(out) > 1) { out$label <- str_c(pars, 1:nrow(out)) }
  return(out)
}
