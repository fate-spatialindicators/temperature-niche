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
wc_index_250$depth = "0 - 250"
goa_index_250$depth = "0 - 250"
bc_index_250$depth = "0 - 250"
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
wc_index_250_500$depth = "250 - 500"
goa_index_250_500$depth = "250 - 500"
bc_index_250_500$depth = "250 - 500"
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
wc_index_500_750$depth = "500 - 750"
goa_index_500_750$depth = "500 - 750"
bc_index_500_750$depth = "500 - 750"
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
  labs(colour = "Depth (m)", fill = "Depth (m)") + 
  scale_fill_viridis_d(option="magma", begin=0.2, end=0.7) + 
  scale_color_viridis_d(option="magma", begin=0.2, end=0.7) + 
  ylab(expression(paste("Temperature index ", degree, "C"))) + 
  theme_bw() + 
  theme(strip.background =element_rect(fill="white"),
        axis.text = element_text(size=8))

#gsave(plot = g1, filename="plots/Figure_S3.png", width = 6, height = 5)


goa <- readRDS("survey_data/joined_goa_data.rds")
goa <- dplyr::filter(goa, species == unique(goa$species)[1])
bc <- readRDS("survey_data/bc_data_2021.rds")
bc <- dplyr::filter(bc, species == unique(bc$species)[1])
cow <- readRDS("survey_data/joined_nwfsc_data.rds")
cow <- dplyr::filter(cow, species == unique(cow$species)[1])
bc$survey <- "BC"
goa$survey <- "GOA"
cow$survey <- "COW"
d <- rbind(goa[,c("survey","depth"),], bc[,c("survey","depth"),], cow[,c("survey","depth"),])
d$survey <- factor(d$survey, levels = c("GOA","BC","COW"))
g2 <- ggplot(d, aes(-depth)) + 
  geom_histogram(aes(y = after_stat(density))) + 
  facet_wrap(~survey, ncol=1) + 
  scale_x_continuous(breaks = c(0,-500,-1000), labels = c("0","500","1000")) + 
  scale_y_continuous(breaks = c(0,0.002, 0.004)) + 
  xlab("Depth (m)") + 
  ylab("Proportion") + 
  coord_flip() + theme_bw() + 
  theme(strip.background =element_rect(fill="white"),
        axis.text = element_text(size=8))

gridExtra::grid.arrange(g1, g2, nrow=1, widths = c(3,1))


g3 <- gridExtra::grid.arrange(g1, g2, nrow=1, widths = c(4,2))
ggsave(g3, filename="plots/Figure_S3.png", width = 6, height = 5)
