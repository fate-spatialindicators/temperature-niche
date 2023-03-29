library(ggplot2)

goa <- readRDS("survey_data/joined_goa_data.rds")
goa_years <- unique(goa$year)

wc_index = readRDS("output/temp_index_wc.rds")
goa_index = readRDS("output/temp_index_goa.rds")
bc_index = readRDS("output/temp_index_bc.rds")
wc_index$region = "COW"
goa_index$region = "GOA"
bc_index$region = "BC"
wc_index$depth = "All"
goa_index$depth = "All"
bc_index$depth = "All"
wc_index = dplyr::filter(wc_index, year >= 2003)
bc_index = dplyr::filter(bc_index, year >= 2003)
goa_index = dplyr::filter(goa_index, year %in% goa_years)

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
wc_index_250 = dplyr::filter(wc_index_250, year >= 2003)
bc_index_250 = dplyr::filter(bc_index_250, year >= 2003)
goa_index_250 = dplyr::filter(goa_index_250, year %in% goa_years)

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
wc_index_250_500 = dplyr::filter(wc_index_250_500, year >= 2003)
bc_index_250_500 = dplyr::filter(bc_index_250_500, year >= 2003)
goa_index_250_500 = dplyr::filter(goa_index_250_500, year %in% goa_years)

# load indices for 0-500m
wc_index_500_750 = readRDS("output/temp_index_wc_500_750.rds")
goa_index_500_750 = readRDS("output/temp_index_goa_500_750.rds")
bc_index_500_750 = readRDS("output/temp_index_bc_500_750.rds")
wc_index_500_750$region = "COW"
bc_index_500_750$region = "BC"
goa_index_500_750$region = "GOA"
wc_index_500_750$depth = "500_750m"
goa_index_500_750$depth = "500_750m"
bc_index_500_750$depth = "500_750m"
wc_index_500_750 = dplyr::filter(wc_index_500_750, year >= 2003)
bc_index_500_750 = dplyr::filter(bc_index_500_750, year >= 2003)
goa_index_500_750 = dplyr::filter(goa_index_500_750, year %in% goa_years)

index = rbind(wc_index, bc_index, goa_index, wc_index_250, 
              wc_index_250_500, goa_index_250, goa_index_250_500,
              bc_index_250, bc_index_250_500)

index = rbind(index, wc_index_500_750, goa_index_500_750, bc_index_500_750)

index$region = as.factor(index$region)
index = dplyr::rename(index, Depth = depth)

index$region = factor(index$region, levels = c("GOA","BC","COW"))

g1 = ggplot(dplyr::filter(index, Depth != "All"), aes(year, est, fill=Depth, col=Depth)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr),alpha=0.3,col=NA) + 
  geom_point(alpha=0.5) + 
  geom_line(alpha=0.5) +
  facet_wrap(~region, ncol = 1, scale="free_y") + 
  xlab("Year") + 
  scale_fill_viridis_d(option="magma", begin=0.2, end=0.7) + 
  scale_color_viridis_d(option="magma", begin=0.2, end=0.7) + 
  ylab(expression(paste("Temperature index ", degree, "C"))) + 
  theme_bw() + 
  theme(strip.background =element_rect(fill="white"))

ggsave(plot = g1, filename="plots/Figure_S3.png", width = 6, height = 5)
