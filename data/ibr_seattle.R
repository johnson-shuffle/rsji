# ----- preample ----------------------------------------------------------


# ----- spd incident data -------------------------------------------------

sibr <- read_csv('raw/ibr_seattle.csv')

# names
names(sibr) %<>% str_replace_all(' ', '_') %>% tolower()
names(sibr)[9:10] <- c('occurred_start', 'occurred_end')

# get ucr codes
ucr <- tbl(db, 'codes_spd') %>% collect()
ucr$offense_code %<>% as.character()
sibr <- left_join(sibr, ucr)

# keep unique go numbers (may have duplicates because of multiple crimes)
sibr %<>% select(-rms_cdw_id) %>% distinct()

# rename
sibr %<>% rename(sector = `district/sector`, beat = `zone/beat`)


# ----- add to database ---------------------------------------------------

copy_to(db, sibr, temporary = F, overwrite = T)
