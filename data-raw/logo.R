#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Name          :logo.R
# Description   :Creates a hex sticker for the drtplanr package.
# Author        :Merlin Unterfinger <info@munterfinger.ch>
# Date          :2020-05-25
# Version       :0.1.0
# Usage         :./logo.R
# Notes         :
# Bash          :5.0.17
# =============================================================================

library(drtplanr)
library(hexSticker)
library(ggplot2)
library(sf)

outfile <- paste0(getwd(), "/man/figures/logo.svg")

# Colors
color1 <- "#393e47"
color2 <- "#C26665"
color3 <- "#FFFFFF"

# Load model
m <- drt_import(
  system.file("Jegenstorf_i1000.RData", package = "drtplanr")
)

# Bounding box for plot
bbox <- m$layer$aoi %>%
  st_transform(2056) %>%
  st_buffer(50) %>%
  st_transform(4326) %>%
  st_bbox()

# Persist station ids
idx <- c(
  859, 297, 480, 937, 799, 422, 595, 577, 831, 581, 522, 950, 141,
  237, 725, 39, 1029, 1078, 764, 134, 910, 890, 94, 1055, 756, 901, 518
)

# Plot
p <-
  ggplot(m$layer$aoi %>%
           st_transform(2056) %>%
           st_buffer(100) %>%
           st_buffer(-75) %>%
           st_transform(4326)) +
  geom_sf(lwd = 0.0, fill = color1) +
  geom_sf(data = m$layer$pop,
          aes(color = n), cex = 1.34, shape = 15, alpha = 0.4) +
  geom_sf(data = m$layer$roa,
          color = color2, lwd = 0.25) +
  geom_sf(data = m$layer$seg[idx, ],
          color = color2, cex = 0.5) +
  geom_sf(data = m$layer$seg[idx, ],
          color = color1, cex = 0.001) +
  theme_void() +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4])) +
  scale_color_gradient(low = color2, high = color1) +
  guides(shape = FALSE, fill = FALSE, color = FALSE)

# Sticker
sticker(p, package = "", p_size = 12,
        s_x = 1, s_y = 1.1, s_width = 1.9, s_height = 1.9,
        h_fill = color1, h_color = color2,
        filename = outfile, white_around_sticker = FALSE,
        url = "drtplanr", u_color = color3,
        u_x = 0.95, u_y = 0.2, u_size = 6, u_angle = 30)
