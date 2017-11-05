# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
library(acs)
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# ------------------------------------------------------------------------------
# acs data
# ------------------------------------------------------------------------------
acs <- tbl(db, 'acs_aff') %>% collect()
acs %<>%
  mutate(
    tract_10 = as.numeric(str_sub(geoid12, 6, 11)),
    bg = as.numeric(str_sub(geoid12, 12, 12))
  )

# ------------------------------------------------------------------------------
# join sectors w/ acs
# ------------------------------------------------------------------------------
#   B00001 - total population
#   B00002 - housing units
#   B01001 - sex by age 
#   B02001 - race (002 = W, 003 = B, 004 = I, 005 = A)
#   B03003 - hispanic (002 = NH, 003 = H)
#   B15003 - education (017 = diploma, 018 = GED, 022 = Bachelors)
#   B17021 - poverty status
#   B19001 - income cohorts
#   B19013 - median income
# ------------------------------------------------------------------------------

# sectors
sect <- tbl(db, 'sectors') %>% collect()
sect %<>% mutate(geoid12 = as.numeric(GEOID))
sect %<>% select(-GEOID)

# join
dat <- left_join(sect, select(acs, geoid12, variable, est))
nrow(sect) * length(unique(dat$variable)) == nrow(dat)

# drop non-count data (median income)
dat %<>% filter(variable != 'B19013_001')

# adjust count
dat %<>% mutate(est = round(pct * as.numeric(est)))

# sum by sector
dat %<>%
  group_by(sector, variable) %>%
  summarise(est = sum(est, na.rm = T))

# spread
dat %<>% spread(variable, est)

# median income calculation
source('analysis/income_interpolate.R')
dat$B19013_001 <- apply(dat[str_subset(names(dat), 'B19001')], 1, inc_int)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
acs_sector <- dat
copy_to(db, acs_sector, temporary = F, overwrite = T)
