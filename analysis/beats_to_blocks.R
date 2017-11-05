# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
library(rgdal)
library(rgeos)
library(sp)
library(maptools)
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# ------------------------------------------------------------------------------
# spatial data
# ------------------------------------------------------------------------------

# seattle bg's
p <- 'https://www.seattle.gov/Documents/Departments/OPCD/Demographics/GeographicFilesandMaps/'
f <- 'SeattleCensusBlocksandNeighborhoodCorrelationFile.xlsx'
download.file(str_c(p, f), destfile = 'raw/tmp.xlsx')
geo <- read_excel('./raw/tmp.xlsx')
names(geo) %<>% tolower()

# block groups
bgs <- readOGR(db_gis, 'blockgroups')
bgs <- spTransform(bgs, '+init=epsg:32610')
bgs <- bgs[bgs$GEOID %in% unique(str_sub(geo$geoid10, 1, 12)), ]

# beats
spd <- readOGR(db_gis, 'beats')
spd <- spTransform(spd, proj4string(blk))

# ------------------------------------------------------------------------------
# sectors
# ------------------------------------------------------------------------------

# sector polygons
spd_sectors <- unionSpatialPolygons(spd, str_sub(spd@data$beat, 1, 1))
spd_sectors <- SpatialPolygonsDataFrame(
  spd_sectors,
  data.frame(sector = names(spd_sectors)),
  match.ID = F
  )

# intersect
spd_bgs <- raster::intersect(spd_sectors, bgs)

# areas by sector and block group
areas <- tibble(ALAND = map_dbl(spd_bgs@polygons, ~slot(.x, 'area')))
row.names(areas) <- map_chr(spd_bgs@polygons, ~slot(.x, 'ID'))
areas <- spCbind(spd_bgs, areas)

# sectors
sectors <- areas@data %>%
  mutate(pct = ALAND.1 / (ALAND + AWATER)) %>%
  select(GEOID, sector, pct)

# ------------------------------------------------------------------------------
# beats
# ------------------------------------------------------------------------------

# beat polygons
spd_beats <- unionSpatialPolygons(spd, spd@data$beat)
spd_beats <- SpatialPolygonsDataFrame(
  spd_beats,
  data.frame(beat = names(spd_beats)),
  match.ID = F
)

# intersect
spd_bgs <- raster::intersect(spd_beats, bgs)

# areas by sector and block group
areas <- tibble(ALAND = map_dbl(spd_bgs@polygons, ~slot(.x, 'area')))
row.names(areas) <- map_chr(spd_bgs@polygons, ~slot(.x, 'ID'))
areas <- spCbind(spd_bgs, areas)

# sectors
beats <- areas@data %>%
  mutate(pct = ALAND.1 / (ALAND + AWATER)) %>%
  select(GEOID, beat, pct)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, sectors, temporary = F, overwrite = T)
copy_to(db, beats, temporary = F, overwrite = T)
