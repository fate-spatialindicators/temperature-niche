library(ggplot2)

wc_index = readRDS("output/temp_index_wc.rds")
goa_index = readRDS("output/temp_index_goa.rds")
goa <- readRDS("survey_data/joined_goa_data.rds")
bc_index = readRDS("output/temp_index_bc.rds")
bc <- readRDS("survey_data/bc_data_2021.rds")
wc_index$region = "COW"
goa_index$region = "GOA"
bc_index$region = "BC"

# need to filter out years without surveys
wc_index <- dplyr::filter(wc_index, year >= 2003, year != 2020)
goa_index <- dplyr::filter(goa_index, year %in% unique(goa$year))
bc_index <- dplyr::filter(bc_index, year %in% unique(bc$year))

index = rbind(wc_index, goa_index, bc_index)
index$Region = factor(index$region, levels = c("GOA","BC","COW"))
g1 = ggplot(dplyr::filter(index, year >= 2003), aes(year, est, fill=Region, col=Region)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr),alpha=0.3,col=NA) + 
  geom_point(alpha=0.5) + 
  geom_line(alpha=0.5) + 
  xlab("Year") + 
  ylab(expression(paste("Temperature index ", degree, "C"))) + 
  theme_bw() + 
  scale_fill_viridis_d(begin = 0.2, end=0.8, option="magma") + 
  scale_color_viridis_d(begin = 0.2, end=0.8, option="magma")

ggsave(plot = g1, filename="plots/Figure_3.png", width = 6, height = 5)
