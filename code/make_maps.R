# devtools::install_github("pbs-assess/sdmTMB", "quadratic")
library(sdmTMB)
library(dplyr)
library(sp)
library(lubridate)

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

library(ggplot2)
library(sf)
library(viridis)
map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", country = c("United States of America","Canada","Mexico"))
# Crop the polygon for plotting and efficiency:
# st_bbox(map_data) # find the rough coordinates
coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = -140, ymin = 30, xmax = -117, ymax = 49))))

#utm_zone9 <- 3156
utm_zone10 <- 3157
coast_proj <- sf::st_transform(coast, crs = utm_zone10)

survey <- dplyr::filter(dat, year == 2015, survey == "COW")

g1 <- ggplot(coast_proj) + geom_sf(fill = "antiquewhite") + 
  theme_bw() + 
  labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = survey, aes(longitude*1000, latitude*1000), alpha=0.5, 
             size=0.5, col=viridis(3, end=0.8)[2])

ggsave(g1, file="cow_map.png")


coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = -160, ymin = 30, xmax = -117, ymax = 60))))
utm_zone10 <- 3157
coast_proj <- sf::st_transform(coast, crs = utm_zone10)

survey <- dplyr::filter(dat, year == 2015)

g1 <- ggplot(coast_proj) + geom_sf(fill = "antiquewhite") + 
  theme_bw() + 
  labs(x = "Longitude", y = "Latitude") + 
  geom_point(data = survey, aes(longitude*1000, latitude*1000, col=survey), alpha=0.5, 
             size=0.5) + 
  scale_color_viridis(end=0.8, discrete=TRUE)
ggsave(g1, file="all_map.png")



