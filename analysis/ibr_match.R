# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# ------------------------------------------------------------------------------
# parse dates, create windows, keep 2015
# ------------------------------------------------------------------------------

# national data
nibr1 <- tbl(db, 'nibr1') %>% collect()
nibr2 <- tbl(db, 'nibr2') %>% collect()

# nibr1 duplicates - incidents with V1006 flag listed twice (keep one w/ hr)
nibr1 %<>% filter(!duplicated(V1004) | V1006 != 'R')

# nibr2 duplicates - incidents with multiple dates (keep most recent)
nibr2$dt <- ymd(nibr2$V2005)
nibr2 %<>% group_by(V2004) %>% mutate(md = max(dt)) %>% ungroup()
nibr2 %<>% filter(dt == md)
nibr2 %<>% select(-dt, -md)

# combine
nibr <- left_join(
  nibr2,
  distinct(nibr1, V1004, V1006, V1007),
  by = c('V2004' = 'V1004')
)
nibr %<>%
  mutate(
    dt = parse_date_time(
      str_c(V2005, str_pad(V1007, 2, 'left', 0)), 
      orders = '%Y%m/%d %H',
      tz = 'US/Pacific'
      ),
    wd = str_c(hour(dt), hour(dt) + 1, sep = '-'),
    month = month(dt),
    year = year(dt),
    day = day(dt)
  )

# seattle data
sibr <- tbl(db, 'sibr') %>% collect()
sibr %<>%
  mutate(
    dt = parse_date_time(
      occurred_start, 
      orders = '%Y-%m-%d %H:%M:%S',
      tz = 'US/Pacific'
    ),
    wd = str_c(hour(occurred_start), hour(occurred_start) + 1, sep = '-'),
    day = day(occurred_start)
  )
sibr %<>% filter(year(occurred_start) %in% 2012:2015)

# spd has duplicates??? (unclear if these are unique, assume they are not)
sibr_names <- names(sibr)[-1]
sibr %<>% unite(id, -rms_cdw_id)
sibr %<>% group_by(id) %>% mutate(r = row_number()) %>% filter(r == max(r))
sibr %<>% select(-r) %>% separate(id, into = sibr_names, sep = '_')

# ------------------------------------------------------------------------------
# candidate matches based on date, window, and crime
# ------------------------------------------------------------------------------
nibr %<>% unite(id, year, month, day, wd, V2006)
sibr %<>% unite(id, year, month, day, wd, ucr_code)

# id's that are duplicated (cannot be matched)
dup_n <- nibr$id[duplicated(nibr$id)] %>% unique()
dup_s <- sibr$id[duplicated(sibr$id)] %>% unique()

# join
mat <- full_join(
  filter(nibr, !id %in% dup_n),
  filter(sibr, !id %in% dup_s),
  by = 'id'
)
mat %<>% filter(!is.na(V2004) & !is.na(general_offense_number))
mat %<>% select(V2004, rms_cdw_id, ucr_cat, `zone/beat`, longitude, latitude)
names(mat)[4] <- 'beat'
mat$sector <- str_sub(mat$beat, 1, 1)

# ------------------------------------------------------------------------------
# join demographic and spatial data
# ------------------------------------------------------------------------------

# arrestee demographics
dem1 <- tbl(db, 'nibr6') %>%
  collect() %>%
  filter(V6004 %in% mat$V2004) %>%
  select(V6004, V6014, V6015, V6016, V6017) %>%
  mutate(type = 'arrest')
names(dem1)[1:5] <- c('id', 'age', 'sex', 'race', 'ethn')

# offender demographics (if no arrestee records)
dem2 <- tbl(db, 'nibr5') %>%
  collect() %>%
  filter(V5004 %in% mat$V2004 & !V5004 %in% dem1$id) %>%
  select(V5004, V5007, V5008, V5009, V5011) %>%
  mutate(type = 'offend')
names(dem2)[1:5] <- c('id', 'age', 'sex', 'race', 'ethn')

dem <- rbind(dem1, dem2) ; rm(dem1, dem2)

# identify individual records, gather, join, spread
mibr <- dem
mibr %<>% group_by(id) %>% mutate(rec = row_number()) %>% ungroup()
mibr %<>% gather(variable, value, -id, -rec, -type)
mibr %<>% left_join(mat, by = c('id' = 'V2004'))
mibr %<>% spread(variable, value)

# add date/time AS CHARACTER! (82856 obs)
mibr %<>% left_join(distinct(nibr, V2004, dt), by = c('id' = 'V2004'))
mibr$dt %<>% as.character()
mibr <- mibr[c(1, 4, 2:3, 5:6, 9, 7, 8, 10:14)]

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, mibr, temporary = F, overwrite = T)
