library(ggplot2)

wc_index = readRDS("output/temp_index_wc.rds")
goa_index = readRDS("output/temp_index_goa.rds")
wc_index$region = "COW"
goa_index$region = "GOA"
wc_index$depth = "All"
goa_index$depth = "All"

# load indices for 0-250m
wc_index_250 = readRDS("output/temp_index_wc_250.rds")
goa_index_250 = readRDS("output/temp_index_goa_250.rds")
wc_index_250$region = "COW"
goa_index_250$region = "GOA"
wc_index_250$depth = "0 - 250"
goa_index_250$depth = "0 - 250"

# load indices for 0-500m
wc_index_250_500 = readRDS("output/temp_index_wc_250_500.rds")
goa_index_250_500 = readRDS("output/temp_index_goa_250_500.rds")
wc_index_250_500$region = "COW"
goa_index_250_500$region = "GOA"
wc_index_250_500$depth = "250 - 500"
goa_index_250_500$depth = "250 - 500"

index = rbind(wc_index, goa_index, wc_index_250, 
              wc_index_250_500, goa_index_250, goa_index_250_500)

index$region = as.factor(index$region)
index = dplyr::rename(index, Depth = depth)
g1 = ggplot(index, aes(year, est, fill=Depth, col=Depth)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr),alpha=0.3,col=NA) + 
  geom_point(alpha=0.5) + 
  geom_line(alpha=0.5) +
  facet_wrap(~region, ncol = 1) + 
  xlab("Year") + 
  scale_fill_viridis_d(end=0.8) + 
  scale_color_viridis_d(end=0.8) + 
  ylab("Estimated temperature index") + 
  theme_bw() + 
  theme(strip.background =element_rect(fill="white"))

ggsave(plot = g1, filename="plots/Figure_S3.png", width = 6, height = 5)
