# devtools::install_github("pbs-assess/sdmTMB", "quadratic")
# devtools::install_github("pbs-assess/gfplot")
library(sdmTMB)
library(dplyr)
library(sp)
library(lubridate)
library(gfplot)

cow <- readRDS("survey_data/joined_nwfsc_data.rds")
# just use haul info -- can be for any spp
cow <- dplyr::filter(cow, scientific_name == scientific_name[1])
cow$date <- lubridate::parse_date_time(cow$date, order = "ymd")
cow$yday <- lubridate::yday(cow$date)

dat <- readRDS("survey_data/bc_data_2021.rds")
dat <- dplyr::select(dat, -start_time, -count, -sensor_name) %>%
  dplyr::rename(temp = temperature)
events <- read.csv("survey_data/synoptic-fishing-event-data.csv") %>%
  dplyr::select(fishing_event_id, month, day)
dat <- dplyr::left_join(dat, events)
dat$yday <- yday(lubridate::parse_date_time(paste(dat$year,dat$month,dat$day), orders = "ymd"))
dat <- dplyr::filter(dat, !is.na(yday))
dat <- dplyr::filter(dat, species == "arrowtooth flounder")

goa <- readRDS("survey_data/joined_goa_data.rds")
goa <- dplyr::filter(goa, scientific_name == "Atheresthes stomias")
goa$date <- lubridate::parse_date_time(goa$date, order = "ymd")
goa$yday <- lubridate::yday(goa$date)

# bind all together
names <- table(c(names(goa), names(dat), names(cow)))
names <- names(which(names==3))
goa = goa[,which(names(goa) %in% names)]
dat = dat[,which(names(dat) %in% names)]
dat$survey <- "BC"
cow = cow[,which(names(cow) %in% names)]
cow$survey <- "COW"

dat <- rbind(dat, cow, goa)

# UTM transformation
dat_ll <- dat
coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
proj4string(dat_ll) <- CRS("+proj=longlat +datum=WGS84")
# convert to utm with spTransform
dat_utm <- spTransform(
  dat_ll,
  CRS("+proj=utm +zone=10 +datum=WGS84 +units=km")
)
# convert back from sp object to data frame
dat <- as.data.frame(dat_utm)
dat <- dplyr::rename(dat,
  longitude = longitude_dd,
  latitude = latitude_dd
)

dat <- dplyr::filter(dat, !is.na(temp), !is.na(depth))
dat$logdepth <- log(dat$depth)

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

#dat$fyear <- as.factor(dat$year)
mu_logdepth <- mean(dat$logdepth) #5.060417
sd_logdepth <- sd(dat$logdepth) #0.689683
dat$logdepth <- (dat$logdepth - mu_logdepth) / sd_logdepth
dat$yday <- scale(dat$yday) # 189.6727,  sd =35.23058

fit <- sdmTMB(temp ~ s(yday) + s(logdepth), 
  mesh = spde,
  time = "year",
  data = dat,
  spatial = "off",
  spatiotemporal = "ar1",
  extra_time = c(1991, 1992, 1994, 1995, 1997, 1998, 2000, 2001, 2002, 2020)
)

# make sure sanity checks pass
#fit <- run_extra_optimization(fit, nlminb_loops = 1)
saveRDS(fit, "all_temp.rds")


# Make predictions for each region
grid <- readRDS("grid_data/wc_grid.rds")
grid <- dplyr::rename(grid, longitude = X, latitude = Y) %>%
  dplyr::mutate(logdepth = log(-depth))

grid$lat_lon <- paste(grid$latitude, grid$longitude)

# scale the grid variables
grid$logdepth_orig <- grid$logdepth
# mu_logdepth = 5.060417, sd_logdepth = 0.689683
grid$logdepth <- (grid$logdepth - mu_logdepth) / sd_logdepth

pred_df <- expand.grid(
  lat_lon = unique(grid$lat_lon),
  year = 1990:2021
)
pred_df <- dplyr::left_join(pred_df, grid)
pred_df$yday <- (182 - 189.6727) / 35.23058 # Day 182 = July 1

# make a prediction for what this will be
pred_temp <- predict(fit, pred_df)
saveRDS(pred_temp, "output/wc_pred_temp.rds")

# Repeat for cells with depth < 250m
sub <- dplyr::filter(pred_df, abs(depth)<250)
pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(sub$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_wc_250.rds")

sub <- dplyr::filter(pred_df, abs(depth)>250, abs(depth) < 500)
pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(sub$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_wc_250_500.rds")

sub <- dplyr::filter(pred_df, abs(depth)>500, abs(depth) < 750)
pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(sub$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_wc_500_750.rds")


# generate temp index for whole coast
pred_temp <- predict(fit, pred_df, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(pred_df$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_wc.rds")


################# GOA grid
grid <- read.csv("grid_data/grid_goa.csv")

grid <- dplyr::rename(grid, 
                      depth = DEPTH_EFH,
                      logdepth = LOG_DEPTH_EFH,
                      latitude_dd = Lat,
                      longitude_dd = Lon) %>% 
  dplyr::select(Id, logdepth, latitude_dd, longitude_dd, Area_km2, depth)

grid_ll <- grid
coordinates(grid_ll) <- c("longitude_dd", "latitude_dd")
proj4string(grid_ll) <- CRS("+proj=longlat +datum=WGS84")
# convert to utm with spTransform
grid_utm <- spTransform(
  grid_ll,
  CRS("+proj=utm +zone=10 +datum=WGS84 +units=km")
)
# convert back from sp object to data frame
grid <- as.data.frame(grid_utm)
grid <- dplyr::rename(grid,
                      longitude = longitude_dd,
                      latitude = latitude_dd
)

grid$lat_lon <- paste(grid$latitude, grid$longitude)

# scale the grid variables
grid$logdepth_orig <- grid$logdepth
# mu_logdepth = 4.847897, sd_logdepth = 0.665218
grid$logdepth <- (grid$logdepth - mu_logdepth) / sd_logdepth

pred_df <- expand.grid(
  lat_lon = unique(grid$lat_lon),
  year = 1990:2021
)
pred_df <- dplyr::left_join(pred_df, grid)
pred_df$yday <- (182 - 189.6727) / 35.23058 # Day 182 = July 1

# make a prediction for what this will be
pred_temp <- predict(fit, pred_df)
saveRDS(pred_temp, "output/goa_pred_temp.rds")

# Repeat for cells with depth < 250m
sub <- dplyr::filter(pred_df, abs(depth)<250)
pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(sub$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_goa_250.rds")

sub <- dplyr::filter(pred_df, abs(depth)>250, abs(depth) < 500)
pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(sub$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_goa_250_500.rds")

sub <- dplyr::filter(pred_df, abs(depth)>500, abs(depth) < 750)
pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(sub$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_goa_500_750.rds")

# generate temp_index
pred_temp <- predict(fit, pred_df, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(pred_df$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_goa.rds")


################# BC grid
data("synoptic_grid")
synoptic_grid$X = synoptic_grid$X * 1000
synoptic_grid$Y = synoptic_grid$Y * 1000
grid <- dplyr::rename(synoptic_grid, longitude = X, latitude = Y) %>%
  dplyr::mutate(logdepth = log(depth))
#Convert the data frame to SpatialPointsDataFrame
coordinates (grid)= ~longitude + latitude
#Assign a projection to it
proj4string(grid) <- CRS("+proj=utm +zone=9")
#Projection transformation
longlats <- spTransform(grid, CRS("+proj=longlat")) 
#Convert it to data frame
longlats.df <- as.data.frame(longlats)
# now convert to utm zone 10
grid_ll <- longlats.df
coordinates(grid_ll) <- c("longitude", "latitude")
proj4string(grid_ll) <- CRS("+proj=longlat +datum=WGS84")
# convert to utm with spTransform
grid_utm <- spTransform(
  grid_ll,
  CRS("+proj=utm +zone=10 +datum=WGS84 +units=km")
)
# convert back from sp object to data frame
grid <- as.data.frame(grid_utm)


grid <- dplyr::select(grid, longitude, latitude, depth, logdepth, cell_area)

grid$lat_lon <- paste(grid$latitude, grid$longitude)

# scale the grid variables
grid$logdepth_orig <- grid$logdepth
# mu_logdepth = 5.611607, sd_logdepth = 0.8952397
grid$logdepth <- (grid$logdepth - mu_logdepth) / sd_logdepth

pred_df <- expand.grid(
  lat_lon = unique(grid$lat_lon),
  year = 1990:2021
)
pred_df <- dplyr::left_join(pred_df, grid)
pred_df$yday <- (182 - 189.6727) / 35.23058 # Day 182 = July 1

# make a prediction for what this will be
pred_temp <- predict(fit, newdata = pred_df)
saveRDS(pred_temp, "output/bc_pred_temp.rds")

# Repeat for cells with depth < 250m
sub <- dplyr::filter(pred_df, abs(depth)<250)
pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(sub$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_bc_250.rds")

sub <- dplyr::filter(pred_df, abs(depth)>250, abs(depth) < 500)
pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(sub$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_bc_250_500.rds")

sub <- dplyr::filter(pred_df, abs(depth)>500, abs(depth) < 750)
pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(sub$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_bc_500_750.rds")

# generate temp index for whole coast
pred_temp <- predict(fit, pred_df, return_tmb_object = TRUE)
index <- get_index(pred_temp, bias_correct = TRUE)
n_cells <- length(unique(pred_df$lat_lon))
index$est <- index$est / n_cells
index$se <- index$se / n_cells
index$lwr <- index$est - 1.96*index$se
index$upr <- index$est + 1.96*index$se
saveRDS(index, "output/temp_index_bc.rds")









# # generate index for depths < 250
# sub <- dplyr::filter(pred_df, abs(depth)<250)
# pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
# index <- get_index(pred_temp)
# n_cells <- length(unique(sub$lat_lon))
# index$est <- index$log_est / n_cells
# index$se <- index$se / n_cells
# index$lwr <- index$est - 1.96*index$se
# index$upr <- index$est + 1.96*index$se
# saveRDS(index, "output/temp_index_wc_250.rds")
# 
# # generate index for depths 250 - 500
# sub <- dplyr::filter(pred_df, abs(depth)>=250, abs(depth) < 500)
# pred_temp <- predict(fit, sub, return_tmb_object = TRUE)
# index <- get_index(pred_temp)
# n_cells <- length(unique(sub$lat_lon))
# index$est <- index$log_est / n_cells
# index$se <- index$se / n_cells
# index$lwr <- index$est - 1.96*index$se
# index$upr <- index$est + 1.96*index$se
# saveRDS(index, "output/temp_index_wc_250_500.rds")
