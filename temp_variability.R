library(dplyr)


temp <- readRDS("all_temp.rds")


pred_temp <- readRDS("output/goa_pred_temp.rds")
goa_ts <- dplyr::group_by(pred_temp, year) %>%
  dplyr::mutate(est_u = est - mean(est)) %>%
  dplyr::summarise(sd = sd(est_u)) %>%
  dplyr::filter(year >=2003)
goa_ts$region <- "GOA"

pred_temp <- readRDS("output/bc_pred_temp.rds")
bc_ts <- dplyr::group_by(pred_temp, year) %>%
  dplyr::mutate(est_u = est - mean(est)) %>%
  dplyr::summarise(sd = sd(est_u)) %>%
  dplyr::filter(year >=2003)
bc_ts$region <- "BC"

pred_temp <- readRDS("output/wc_pred_temp.rds")
wc_ts <- dplyr::group_by(pred_temp, year) %>%
  dplyr::mutate(est_u = est - mean(est)) %>%
  dplyr::summarise(sd = sd(est_u)) %>%
  dplyr::filter(year >=2003)
wc_ts$region <- "COW"

d <- rbind(goa_ts, bc_ts, wc_ts)

d <- dplyr::filter(d, paste(region, year) %in% paste(temp$data$survey))

ggplot(d, aes(year, sd, col = region)) + 
  geom_point() + 
  geom_line()


