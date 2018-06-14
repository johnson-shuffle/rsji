source('~/.Rprofile')

.First <- function() {
  library(stats)
  library(RSocrata)
  load_tidy()
  db     <<- src_sqlite('~/Projects/rsji/rsji.sqlite', create = F)
  db_gis <<- src_sqlite('~/Projects/rsji/rsji_gis.sqlite', create = F)
}