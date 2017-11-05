# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# ------------------------------------------------------------------------------
# american fact finder
# ------------------------------------------------------------------------------
f <- list.files('raw/aff_download', pattern = 'ann.csv$', full.names = T)
tabs <- str_extract_all(f, 'B\\d{5}', simplify = T)[, 1]

acs <- list()
for (i in seq_along(f)) {
  dat <- read_csv(f[i], skip = 1)
  x <- ncol(dat) - 3
  names(dat)[1:3] <- c('geoid', 'geoid12', 'geography')
  if (i <= 2) {
    n <- str_c(tabs[i], '_', str_pad(1:x, 3, 'left', 0), 'E')
  } else {
    n <- str_c(tabs[i], '_', str_pad(1:(x / 2), 3, 'left', 0))
    n <- map(n, rep , 2) %>% unlist()
    n <- str_c(n, rep(c('E', 'M'), x / 2))
  }
  names(dat)[-1:-3] <- n
  acs[[i]] <- dat
  rm(i, dat, x, n)
}

acs_aff <- acs[[1]]
for (j in seq_along(f)[-1]) {
  acs_aff %<>% full_join(acs[[j]])
  rm(j)
}

acs_aff %<>% gather(variable, value, -1:-3)
acs_aff %<>%
  mutate(
    type = if_else(str_sub(variable, 11, 11) == 'E', 'est', 'moe'),
    variable = str_sub(variable, 1, 10)
    )
acs_aff %<>% spread(type, value) %>% distinct()

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, acs_aff, temporary = F, overwrite = T)
