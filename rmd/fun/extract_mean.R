extract_mean <- function(m, labs = NULL) {
  fs <- summary(fit[[m]]$result)$summary
  pm <- str_extract_all(row.names(fs), '[^\\[\\d\\]]*')
  pm %<>% unlist() %>% unique()
  pm <- pm[!pm %in% c('', 'lp', 'lp__')]
  
  out <- list()
  for (p in seq_along(pm)) {
    val <- fs[, 'mean'][str_detect(row.names(fs), pm[p])]
    race <- NA
    sect <- NA
    if (length(val) == 4) { race <- labs$r }
    if (length(val) == 17) { sect <- labs$s }
    if (length(val) == 68) {
      race <- rep(labs$r, 17)
      sect <- map(labs$s, rep, 4) %>% unlist()
    }
    out[[p]] <- tibble(
      param = pm[p],
      model = m,
      race = race,
      sector = sect,
      value = val)
  }
  
  do.call(rbind, out)
}