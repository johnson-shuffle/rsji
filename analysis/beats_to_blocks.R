# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
library(rgdal)
load_tidy()

# databases
db <- src_sqlite('rsji.sqlite', create = F)
db_gis <- 'rsji_gis.sqlite'

# ------------------------------------------------------------------------------
# seattle blockgroups file
# ------------------------------------------------------------------------------

# link and download
pag  <- 'https://www.seattle.gov/Documents/Departments/OPCD/Demographics/GeographicFilesandMaps/'
file <- 'SeattleCensusBlocksandNeighborhoodCorrelationFile.xlsx'
download.file(str_c(pag, file), destfile = 'raw/tmp.xlsx')

# read and tidy up
geo <- read_excel('./rsji/raw/tmp.xlsx')
names(geo) %<>% tolower()

# blockgroups and blocks
s_groups <- str_sub(s_blocks, 1, 12) %>% unique()
s_blocks <- geo$geoid10 %>% unique() %>% as.character() 

# add beat columns
geo$beat_bl <- NA
geo$beat_bg <- NA

# ------------------------------------------------------------------------------
# spatillay compare seattle blocks with spd beats
# ------------------------------------------------------------------------------
spd <- readOGR(db_gis, 'beats')
blk <- readOGR(db_gis, 'blocks')

# function to check a beat against an individual block
sContains <- function(beat, block, pb = NULL) {
  if (!is.null(pb)) pb$tick()$print()
  gContains(spd[spd$beat == beat, ], blk[blk$GEOID10 == block, ])
  
}

# beats against blocks
for (b in beats$beat) {
  pb <- progress_estimated(length(s_blocks))
  ck <- map_lgl(s_blocks, ~sContains(b, .x, pb = pb))
  ck <- s_blocks[ck]
  geo$beat_bl[geo$geoid10 %in% ck] <- b
}

# get beats/sector/precinct
spd_beats <- spd_beats@data
beats <- left_join(geo, spd_beats[c(2, 3)], by = c('beat_bl' = 'beat'))
beats %<>% rename(beats, prec = first_prec)
beats$sector <- str_sub(beats$beat_bl, 1, 1)

# calculate percentage of blocks within particular beat
beats %<>%
  group_by(tract_10, bg) %>%
  mutate(n_tbl = n()) %>% # blocks within blockgroup
  group_by(tract_10, bg, sector) %>%
  mutate(n_sbl = n()) %>% # blocks within sector
  group_by(tract_10, bg, prec) %>%
  mutate(n_pbl = n()) %>% # blocks within precinct
  ungroup() %>%
  mutate(
    pct_sbl = n_sbl / n_tbl,
    pct_pbl = n_pbl / n_tbl
    )

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, beats, temporary = F, overwrite = T)
