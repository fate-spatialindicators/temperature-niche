library(dplyr)
library(ggplot2)

temp <- readRDS("all_temp.rds")

pred_temp_goa <- readRDS("output/goa_pred_temp.rds")
goa_ts <- dplyr::group_by(pred_temp_goa, year) %>%
  dplyr::mutate(est_u = est - mean(est)) %>%
  dplyr::summarise(sd = sd(est_u)) %>%
  dplyr::filter(year >=2003)
goa_ts$region <- "GOA"

pred_temp_bc <- readRDS("output/bc_pred_temp.rds")
bc_ts <- dplyr::group_by(pred_temp_bc, year) %>%
  dplyr::mutate(est_u = est - mean(est)) %>%
  dplyr::summarise(sd = sd(est_u)) %>%
  dplyr::filter(year >=2003)
bc_ts$region <- "BC"

pred_temp_wc <- readRDS("output/wc_pred_temp.rds")
wc_ts <- dplyr::group_by(pred_temp_wc, year) %>%
  dplyr::mutate(est_u = est - mean(est)) %>%
  dplyr::summarise(sd = sd(est_u)) %>%
  dplyr::filter(year >=2003)
wc_ts$region <- "COW"

# combine all predictions
pred_temp_goa$region <- "GOA"
pred_temp_bc$region <- "BC"
pred_temp_wc$region <- "COW"
all_pred <- rbind(dplyr::select(pred_temp_goa, year, est, region), 
                  dplyr::select(pred_temp_bc, year, est, region), 
                  dplyr::select(pred_temp_wc, year, est, region))
all_pred<- dplyr::filter(all_pred, year %in% seq(2003,2021,2))
alld <- dplyr::group_by(all_pred, year) %>%
  dplyr::summarise(sd = sd(est))
alld$sd <- alld$sd / alld$sd[1]


goa_ts <- dplyr::filter(goa_ts, year %in% seq(2003,2021,2))
bc_ts <- dplyr::filter(bc_ts, year !=2020)
wc_ts <- dplyr::filter(wc_ts, year !=2020)
d <- rbind(goa_ts, bc_ts, wc_ts)

# scale by 1st year
d <- dplyr::group_by(d, region) %>%
  dplyr::mutate(scaled_sd = sd / sd[1])

d$Region = factor(d$region, levels = c("GOA","BC","COW"))

ggplot(d, aes(year, scaled_sd, col = Region)) + 
  geom_point(size=3) + 
  geom_line(linewidth=1.3) + 
  theme_bw() + xlab("Year") + ylab(expression("Normalized" ~ sigma)) + 
  scale_fill_viridis_d(begin = 0.2, end=0.8, option="magma") + 
  scale_color_viridis_d(begin = 0.2, end=0.8, option="magma") + 
  geom_line(data = alld, aes(year,sd), col="grey30", linewidth=2) + 
  geom_point(data = alld, aes(year,sd), col="grey30", size=3)
ggsave(filename="plots/Figure_S6.png", width = 6, height = 5)



