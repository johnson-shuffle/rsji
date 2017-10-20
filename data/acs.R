# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
library(acs)
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# ------------------------------------------------------------------------------
# census api
# ------------------------------------------------------------------------------
b <- tbl(db, 'beats') %>%
  distinct(tract_10, bg) %>%
  arrange(tract_10, bg) %>%
  collect()

# geographic queries
q1 <- geo.make(
  state = 'WA',
  county = 'King',
  tract = b$tract_10,
  block.group = b$bg,
  check = F
)
q2 <- geo.make(
  state = 'WA',
  place = 'Seattle',
  check = F
)
q <- q1 + q2

# fetch (slow!!!)
acs_01001 <- acs.fetch(2015, geography = q, table.number = 'B01001') # age
acs_02001 <- acs.fetch(2015, geography = q, table.number = 'B02001') # race
acs_03003 <- acs.fetch(2015, geography = q, table.number = 'B03003') # hispanic
acs_15002 <- acs.fetch(2015, geography = q, table.number = 'B15002') # educ
acs_19001 <- acs.fetch(2015, geography = q, table.number = 'B19001') # inc dis
acs_19013 <- acs.fetch(2015, geography = q, table.number = 'B19013') # inc med

# put in list
acs_query <- list(
  acs_01001,
  acs_02001,
  acs_03003,
  acs_15002,
  acs_19001,
  acs_19013
)
names(acs) <- 
  str_c('acs_', c('01001', '02001', '03003', '15002', '19001', '19013'))

# save
#save(acs_query, file = './rsji/data/acs_queries.Rda')

# ------------------------------------------------------------------------------
# tidy up the census data
# ------------------------------------------------------------------------------
load('./rsji/data/acs_queries.Rda')

acs_to_tibble <- function(x) {
  tmp1 <- as_tibble(cbind(geography(x), estimate(x)))
  tmp2 <- as_tibble(cbind(geography(x), standard.error(x)))
  tmp1 %<>% gather(variable, est, -1:-6)
  tmp2 %<>% gather(variable, moe, -1:-6)
  tmp1$moe <- tmp2$moe
  return(tmp1)
}

# convert acs to tibble
acs <- map(acs_query, acs_to_tibble)
acs <- do.call(rbind, acs)
acs %<>% mutate_at(vars(state, county, blockgroup), as.numeric)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, acs, temporary = F, overwrite = T)

# ------------------------------------------------------------------------------
# join precincts w/ acs and re-calculate totals
# ------------------------------------------------------------------------------
dat <- tbl(db, 'beats') %>% distinct(prec, beat_bg, beat_bl, tract_10, bg, pct)
dat %<>% collect()
dat %<>% left_join(acs_est, by = c('tract_10' = 'tract', 'bg' = 'blockgroup'))

# keeep sectors, precinctspercentages, and acs variables
dat <- dat[c('prec', 'pct', str_subset(names(dat), 'B\\d{5}_\\d{3}'))]
dat <- filter(dat, !is.na(prec))
dat <- gather(dat, acs_var, value, 3:ncol(dat))
dat <- dat %>%
  mutate(value = round(pct * value)) %>%
  group_by(prec, acs_var) %>%
  summarise(value = sum(value, na.rm = T))
dat <- spread(dat, acs_var, value)

dat$med_inc <- NA
for (i in 1:nrow(dat)) {
  dat$med_inc[i] <- median_income(dat[i, str_subset(names(dat), 'B19001')])
}

# convert race, hispanic, and education to percentages
educ1 <- str_c('B15002_', str_pad(c(12:18, 29:35), 3, pad = 0))
educ4 <- str_c('B15002_', str_pad(c(15:18, 32:35), 3, pad = 0))
dat <- dat %>%
  within({
    black <- B02001_003 / B02001_001
    hispc <- B03003_003 / B03003_001
    educ1 <- rowSums(dat[educ1]) / B15002_001
    educ4 <- rowSums(dat[educ4]) / B15002_001
  })
