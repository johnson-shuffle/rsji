# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()

# databases
db <- src_sqlite('./rsji/rsji.sqlite', create = F)
db_gis <- './rsji/rsji_gis.sqlite'

# age breaks in acs
ages <- c(seq(9, 84, 5), 0, 5, 17, 20, 21, 61, 66, Inf) %>% sort()

# ------------------------------------------------------------------------------
# nibr/sibr matched records
# ------------------------------------------------------------------------------
mibr <- tbl(db, 'mibr') %>%
  collect() %>%
  mutate(
    dt = parse_date_time(dt, '%Y-%m-%d %H:%M:%S'),
    race = if_else(race == '', 'U', race),
    race2 = 'NW',
    race2 = if_else(race == 'W', 'W', race2)
    )

dat1 <- mibr %>%
  filter(year(dt) == 2015) %>%
  #filter(race != 'U') %>%
  filter(sector != '9') %>%
  count(sector, race2) %>%
  group_by(sector) %>%
  mutate(
    n_tot = sum(n),
    pct = n / n_tot
    ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# american community survey
# ------------------------------------------------------------------------------
# B02001: race
#   001 = total, 002 = W, 003 = B, 004 = I, 005 = A
# B03003: hispanic / latino
#   001 = total, 002 = N, 003 = H
# ------------------------------------------------------------------------------

# acs table
acs <- tbl(db, 'acs') %>% collect()

# race variables
acs %<>% filter(variable %in% c(str_c('B02001_00', 2:5)))

# sectors/blocks
beats <- tbl(db, 'beats') %>% collect()

# join
dat2 <- distinct(beats, tract_10, bg, sector, pct_sbl) ; tb <- table(dat2$sector)
dat2 %<>% left_join(acs, c('tract_10' = 'tract', 'bg' = 'blockgroup'))
dat2 %<>% mutate(sest = round(pct_sbl * est))

# check - observations per sector (stored in tb) times 6 variables
all.equal(tb * 4, table(dat2$sector))

# summarise
dat2 %<>%
  filter(!is.na(sector)) %>%
  group_by(sector, variable) %>%
  summarise(n = sum(est)) %>%
  rename(race = variable)
dat2$race2 <- factor(dat2$race)
levels(dat2$race2) <- c('W', 'NW', 'NW', 'NW')
dat2 %<>%
  group_by(sector) %>%
  mutate(
    n_tot = sum(n),
    pct = n / n_tot
  ) %>%
  ungroup()

dat <- left_join(
  select(dat2, -race), dat1,
  by = c('sector', 'race2'),
  suffix = c('_acs', '_ibr')
  )
dat %<>% gather(variable, value, -sector, -race2)
dat %<>% filter(str_sub(variable, 1, 3) == 'pct')
dat %<>% mutate(value = replace(value, is.na(value), 0 ))
brks <- classIntervals(dat$value, n = 5, style = 'jenks')$brks
dat %<>% mutate(value_nat = cut(value, breaks = brks, include.lowest = T))
dat$value_nat %<>% fct_rev()

cr <- colorRampPalette(c('darkblue', 'lightblue'))(5)
p <- ggplot(dat) +
  geom_tile(aes(sector, race2, fill = value_nat)) +
  scale_fill_manual(values = cr) +
  facet_grid(~variable)
p

