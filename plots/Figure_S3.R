library(ggplot2)

wc_index = readRDS("output/temp_index_wc.rds")
goa_index = readRDS("output/temp_index_goa.rds")
bc_index = readRDS("output/temp_index_bc.rds")
wc_index$region = "COW"
goa_index$region = "GOA"
bc_index$region = "BC"
wc_index$depth = "All"
goa_index$depth = "All"
bc_index$depth = "All"

# load indices for 0-250m
wc_index_250 = readRDS("output/temp_index_wc_250.rds")
goa_index_250 = readRDS("output/temp_index_goa_250.rds")
bc_index_250 = readRDS("output/temp_index_bc_250.rds")
wc_index_250$region = "COW"
goa_index_250$region = "GOA"
bc_index_250$region = "BC"
wc_index_250$depth = "0 - 250m"
goa_index_250$depth = "0 - 250m"
bc_index_250$depth = "0 - 250m"

# load indices for 0-500m
wc_index_250_500 = readRDS("output/temp_index_wc_250_500.rds")
goa_index_250_500 = readRDS("output/temp_index_goa_250_500.rds")
bc_index_250_500 = readRDS("output/temp_index_bc_250_500.rds")
wc_index_250_500$region = "COW"
bc_index_250_500$region = "BC"
goa_index_250_500$region = "GOA"
wc_index_250_500$depth = "250 - 500m"
goa_index_250_500$depth = "250 - 500m"
bc_index_250_500$depth = "250 - 500m"

index = rbind(wc_index, bc_index, goa_index, wc_index_250, 
              wc_index_250_500, goa_index_250, goa_index_250_500,
              bc_index_250, bc_index_250_500)

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
  ylab(expression(paste("Temperature index ", degree, "C"))) + 
  theme_bw() + 
  theme(strip.background =element_rect(fill="white"))

ggsave(plot = g1, filename="plots/Figure_S3.png", width = 6, height = 5)
