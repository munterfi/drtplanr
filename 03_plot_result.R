# ------------------------------------------------------------------------------
# drtplanr: Visualize results of the model run
#
# File: ~/drtplanr/03_plot_results.R
# Author: Merlin Unterfinger (info@munterfinger.ch)
# Date: 2020-03-07
#
# GNU General Public License v3.0
# ------------------------------------------------------------------------------

## Load drtplanr
source("R/drtplanr.R")
library(ggplot2)
library(mapview)


## Load results
load("result/model_results.RData")
s_0 <- results$sta_const
s_1 <- results$sta_init
s_x <- results$sta_final
e <- results$energy


## Plotting
tmessage("Plot station map")
m <-
  mapview(s_0, alpha = 0, cex = 6, col.region = "black", layer.name = "Existing") +
  mapview(s_1, alpha = 0, cex = 3, col.region = "blue", layer.name = "Inital") +
  mapview(s_x, alpha = 0, cex = 6, col.region = "red", layer.name = "Optimized")
m

tmessage("Plot energy graph")
p <-
  ggplot(e, aes(x = iteration, y = value)) +
  geom_line() + 
  xlab("Iteration") +
  ylab("Minutes/Population") +
  ggtitle("Station optimization energy") +
  theme_minimal()
p


## Save
tmessage("Export graphics")
mapshot(m, file = paste0(getwd(), "/docs/station_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))
ggsave(paste0(getwd(), "/docs/model_energy.png"), plot = p,
       height = 125.984, width = 100, units = c("mm"), dpi = 150)
