## Load all components of the drtplanr

## PKG
library(sf)
library(data.table)

## SRC
source("R/drtm.R")

## drtplanr core functions
tmessage <- function(text) {
  message(Sys.time(), " ", text)
}
