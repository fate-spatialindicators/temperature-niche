library(ggplot2)


wc_index = readRDS("output/temp_index_wc.rds")
goa_index = readRDS("output/temp_index_goa.rds")
wc_index$region = "COW"
goa_index$region = "GOA"

index = rbind(wc_index, goa_index)
index$Region = as.factor(index$region)
g1 = ggplot(index, aes(year, est, fill=Region, col=Region)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr),alpha=0.3,col=NA) + 
  geom_point(alpha=0.5) + 
  geom_line(alpha=0.5) + 
  xlab("Year") + 
  ylab("Estimated temperature index") + 
  theme_bw() + 
  scale_fill_viridis_d(end=0.8) + 
  scale_color_viridis_d(end=0.8)

ggsave(plot = g1, filename="plots/Figure_5.png", width = 6, height = 5)
