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

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, terry, temporary = F, overwrite = T)
