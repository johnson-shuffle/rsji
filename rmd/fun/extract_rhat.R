extract_rhat <- function(m) {
  tibble(
    model = m,
    param = row.names(summary(fit[[m]]$result)$summary),
    rhat  = summary(fit[[m]]$result)$summary[, 'Rhat']
  )
}