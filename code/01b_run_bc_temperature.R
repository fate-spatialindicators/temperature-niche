#devtools::install_github("pbs-assess/sdmTMB", "quadratic-roots")
#devtools::install_github("pbs-assess/gfplot")
library(sdmTMB)
library(dplyr)
library(sp)
library(lubridate)
library(gfplot)
dat <- readRDS("survey_data/bc_data_2021.rds")
dat <- dplyr::select(dat, -start_time, -count, -sensor_name) %>%
  dplyr::rename(temp = temperature)

events <- read.csv("survey_data/synoptic-fishing-event-data.csv") %>%
  dplyr::select(fishing_event_id, month, day)
dat <- dplyr::left_join(dat, events)
dat$yday <- yday(lubridate::parse_date_time(paste(dat$year,dat$month,dat$day), orders = "ymd"))
dat <- dplyr::filter(dat, !is.na(yday))

# UTM transformation
dat_ll <- dat
coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
proj4string(dat_ll) <- CRS("+proj=longlat +datum=WGS84")
# convert to utm with spTransform
dat_utm <- spTransform(
  dat_ll,
  CRS("+proj=utm +zone=9 +datum=WGS84 +units=km")
)
# convert back from sp object to data frame
dat <- as.data.frame(dat_utm)
dat <- dplyr::rename(dat,
  longitude = longitude_dd, latitude = latitude_dd
)

dat <- dplyr::filter(dat, species == species[1])

dat <- dplyr::filter(dat, !is.na(temp), !is.na(depth))
dat$logdepth <- log(dat$depth)

# include date
# dat$date <- lubridate::parse_date_time(dat$date,order="ymd")
# dat$yday <- lubridate::yday(dat$date)

# make mesh
spde <- try(make_mesh(dat, c("longitude", "latitude"),
  cutoff = 20
), silent = TRUE)

priors <- sdmTMBpriors(
  matern_s = pc_matern(
    range_gt = 5, range_prob = 0.05,
    sigma_lt = 25, sigma_prob = 0.05
  )
)

dat$fyear <- as.factor(dat$year)
mu_logdepth <- mean(dat$logdepth) # 5.039356
sd_logdepth <- sd(dat$logdepth) # 0.6625063
dat$logdepth <- (dat$logdepth - mu_logdepth) / sd_logdepth
mu_yday = mean(dat$yday)
sd_yday = sd(dat$yday)
dat$yday <- scale(dat$yday)

fit <- sdmTMB(temp ~ s(yday) + s(logdepth), # s(yday) + s(logdepth),
              # time_varying ~ logdepth + I(logdepth^2) + I(logepth^3),
              mesh = spde,
              time = "year",
              data = dat,
              # priors=priors,
              spatial = "off",
              spatiotemporal = "iid"
)

data("synoptic_grid")

grid <- dplyr::rename(synoptic_grid, longitude = X, latitude = Y) %>%
  dplyr::mutate(logdepth = log(depth))
grid <- add_utm_columns(grid, ll_names = c("longitude","latitude"))
grid <- dplyr::select(grid, longitude, latitude, depth, logdepth, X, Y, cell_area)

grid$lat_lon <- paste(grid$latitude, grid$longitude)
#grid$latitude <- grid$latitude * 10
#grid$longitude <- grid$longitude * 10

# scale the grid variables
grid$logdepth_orig <- grid$logdepth
# mu_logdepth = 5.611607, sd_logdepth = 0.8952397
grid$logdepth <- (grid$logdepth - mu_logdepth) / sd_logdepth

pred_df <- expand.grid(
  lat_lon = unique(grid$lat_lon),
  year = unique(dat$year)
)
pred_df <- dplyr::left_join(pred_df, grid)
pred_df$yday <- (182 - mu_yday) / sd_yday # Day 182 = July 1

# make a prediction for what this will be
pred_temp <- predict(fit, newdata = pred_df)
saveRDS(pred_temp, "output/bc_pred_temp.rds")

# also generate samples to propagate uncertainty
pred_temp <- predict(fit, pred_df, nsim = 100)
saveRDS(pred_temp, "output/bc_pred_temp_uncertainty.rds")
