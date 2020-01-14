library(ggplot2)
library(mapview)

load("result/model_results.RData")
s_0 <- results$sta_const
s_1 <- results$sta_init
s_x <- results$sta_final
e <- results$energy

# Plotting
m <-
  mapview(s_0, alpha = 0, cex = 4, col.region = "black", layer.name = "Existing") +
  mapview(s_1, alpha = 0, cex = 2, col.region = "blue", layer.name = "Inital") +
  mapview(s_x, alpha = 0, cex = 4, col.region = "red", layer.name = "Final")
m

p <-
  ggplot(e, aes(x = iteration, y = value)) +
  geom_line() + 
  xlab("Iteration") +
  ylab("Minutes/Population") +
  ggtitle("Station optimization energy") +
  theme_minimal()
p

# Save
mapshot(m, file = paste0(getwd(), "/docs/station_map.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton"))
ggsave(paste0(getwd(), "/docs/model_energy.png"), plot = p,
       height = 125.984, width = 100, units = c("mm"), dpi = 150)
