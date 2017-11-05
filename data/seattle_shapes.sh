#!/bin/bash  

# ------------------------------------------------------------------------------
# shapefile for police beats (manual download)
# ------------------------------------------------------------------------------
# data.seattle.gov/Public-Safety/Seattle-Police-Department-Beats/nnxn-434b

# ------------------------------------------------------------------------------
# r code to get seattle shapefiles via tigris
# ------------------------------------------------------------------------------
echo -e "library(tigris) ; options(tigris_use_cache = T); \
    setwd('~/Projects/rsji/raw'); \
    places('WA'); \
    block_groups('WA', 'King'); \
    blocks('WA', 'King')" \
    | R --no-save

# ------------------------------------------------------------------------------
# locations
# ------------------------------------------------------------------------------

# database
db=~/Projects/rsji/rsji.sqlite
db_gis=~/Projects/rsji/rsji_gis.sqlite

# ------------------------------------------------------------------------------
# shapefiles
# ------------------------------------------------------------------------------

# location of zipfiles
cd ~/Projects/rsji/raw/

# file names
f=("SPD_BEATS_WGS84" "tl_2015_53_place" "tl_2015_53_bg")
f=(${f[@]} "tl_2015_53_tabblock10")

# table names
t=("beats" "places" "blockgroups" "blocks")

# epsg parameters
epsg=(3857 4269 4269 4269)

# add to gis database
for i in `seq 0 1 3`; do
    unzip ${f[$i]} -d ${f[$i]}
    echo -e ".loadshp ${f[$i]}/${f[$i]} ${t[$i]} utf-8 ${epsg[$i]}" \
        | spatialite $db_gis
    rm -rf ${f[$i]}
done
