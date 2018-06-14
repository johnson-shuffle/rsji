# ----- preample ----------------------------------------------------------


# ----- spd open portal data (data.seattle.gov) ---------------------------

# login
spd_token <- Sys.getenv('SPD_TOKEN')
spd_credi <- c(Sys.getenv('SPD_USER'), Sys.getenv('SPD_PASS'))

# api's
apis <- tbl(db, 'apis') %>% collect()


# ----- terry stops -------------------------------------------------------

terry <- read.socrata(
  filter(apis, api == 'terry')$link,
  app_token = spd_token,
  email = spd_credi[1],
  password  = spd_credi[2]
)


# ----- tidy up -----------------------------------------------------------

# race
terry$subjectrace %<>% as.factor()
levels(terry$subjectrace) <- c('U', 'I', 'A', 'B', 'H', 'M', 'O', 'U', 'W')
terry$subjectrace %<>% as.character()

# gender
terry$subjectgender %<>% as.factor()
levels(terry$subjectgender) <- c('U', 'F', 'M', 'U')
terry$subjectgender %<>% as.character()

# date/time
date_reported <- str_split(terry$reported_time, '\\.', simplify = T)[, 1]
terry$date_reported <- str_c(terry$reported_date, date_reported, sep = ' ')


# ----- add to database ---------------------------------------------------

copy_to(db, terry, temporary = F, overwrite = T)
