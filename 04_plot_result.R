library(ggplot2)
library(mapview)

load("result/model_results.RData")
s_0 <- results$sta_const
#s_0$id <- seq(1, nrow(s_0))
s_1 <- results$sta_init
#s_1$id <- seq(1, nrow(s_1))
s_x <- results$sta_final
#s_x$id <- seq(1, nrow(s_x))

e <- results$energy

# Plotting
m <-
  mapview(s_0, alpha = 0, cex = 4, col.region = "black", layer.name = "Existing") +
  mapview(s_1, alpha = 0, cex = 2, col.region = "blue", layer.name = "Inital") +
  mapview(s_x, alpha = 0, cex = 4, col.region = "red", layer.name = "Final")
m

ggplot(e, aes(x = iteration, y = value)) +
  geom_line() + 
  xlab("Iteration") +
  ylab("Energy: Minutes/Population") +
  ggtitle("Station optimization") +
  theme_minimal()
