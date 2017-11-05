# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
library(RSocrata)
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# ------------------------------------------------------------------------------
# spd open portal data
#   https://data.seattle.gov/browse?category=Public+Safety&provenance=official
# ------------------------------------------------------------------------------

# login
spd_token <- '1KmPMFsPnJEpif9JBhaNWIIVl'
spd_credi <- c('jake.j.is@gmail.com', 'wR:g=v\"5(bMX:Tt\'')

# api's
apis <- tbl(db, 'apis') %>% collect()

# ------------------------------------------------------------------------------
# terry stops
# ------------------------------------------------------------------------------
terry <- read.socrata(
  filter(apis, api == 'terry')$link,
  app_token = spd_token,
  email = spd_credi[1],
  password  = spd_credi[2]
)

# tidy up race
terry$subjectrace %<>% as.factor()
levels(terry$subjectrace) <- c('U', 'I', 'A', 'B', 'H', 'M', 'O', 'U', 'W')
terry$subjectrace %<>% as.character()

# tidy up gender
terry$subjectgender %<>% as.factor()
levels(terry$subjectgender) <- c('U', 'F', 'M', 'U')
terry$subjectgender %<>% as.character()

# report date/time
date_reported <- str_split(terry$reported_time, '\\.', simplify = T)[, 1]
terry$date_reported <- str_c(terry$reported_date, date_reported, sep = ' ')

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, terry, temporary = F, overwrite = T)
