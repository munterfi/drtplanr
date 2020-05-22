#!/usr/bin/env bash
# ----------------------------------------------------------------------------- 
# Name          :01_download_extract.sh
# Description   :Downloads and extracts the example datasets.
# Author        :Merlin Unterfinger <info@munterfinger.ch>
# Date          :2020-05-22
# Version       :0.1.0  
# Usage         :./01_download_extract.sh
# Notes         :Run from the 'data-raw' folder, before executing the R files.       
# Bash          :5.0.17 
# =============================================================================

# Get STATPOP data set
mkdir -p statpop && cd "$_"
curl https://www.bfs.admin.ch/bfsstatic/dam/assets/9947069/master -o statpop.zip
unzip -a statpop.zip
rm statpop.zip && cd -

# Get OSM cutout
mkdir -p osm && cd "$_"
curl https://download.geofabrik.de/europe/switzerland-latest-free.shp.zip -o osm.zip
unzip -a osm.zip
rm osm.zip && cd -

