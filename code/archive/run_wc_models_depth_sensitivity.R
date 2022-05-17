devtools::install_github("pbs-assess/sdmTMB")
library(sdmTMB)
library(dplyr)
library(sp)

dat <- readRDS("survey_data/joined_nwfsc_data.rds")

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

# use criteria based on sample sizes and depth
sp_to_drop <- dplyr::group_by(dat, species, year) %>%
  dplyr::summarize(
    p = length(which(cpue_kg_km2 > 0)) / n(),
    n = length(which(cpue_kg_km2 > 0)),
    mean_depth = mean(depth[which(cpue_kg_km2 > 0)], na.rm = T)
  ) %>%
  dplyr::group_by(species) %>%
  summarize(
    n_year = length(which(p > 0)), p = mean(p),
    min_n = min(n), mean_n = mean(n),
    mean_depth = mean(mean_depth, na.rm = T)
  ) %>%
  dplyr::filter(mean_depth <= 400, n_year == 16)

dat <- dplyr::filter(dat, species %in% sp_to_drop$species == FALSE)

df <- expand.grid(
  "species" = unique(dat$species),
  spatial_only = c(FALSE),
  depth_effect = c(TRUE, FALSE),
  time_varying = c(FALSE),
  covariate = c("temp")
)
saveRDS(df, "output/wc/models.RDS")

depth_filter <- TRUE

for (i in 1:nrow(df)) {

  # filter by species, and select range within occurrences
  sub <- dplyr::filter(
    dat,
    species == df$species[i]
  ) %>%
    dplyr::filter(
      latitude > min(latitude[which(cpue_kg_km2 > 0)]),
      latitude <= max(latitude[which(cpue_kg_km2 > 0)]),
      longitude > min(longitude[which(cpue_kg_km2 > 0)]),
      longitude < max(longitude[which(cpue_kg_km2 > 0)])
    )

  if (depth_filter == TRUE) {
    sub <- dplyr::mutate(sub,
      min_depth = quantile(depth, 0.25, na.rm = T),
      max_depth = quantile(depth, 0.75, na.rm = T)
    ) %>%
      dplyr::filter(depth <= max_depth, depth >= min_depth)
  }

  # rescale variables
  sub$depth <- scale(log(sub$depth))
  sub$o2 <- scale(log(sub$o2))
  sub$temp <- scale(sub$temp)

  # drop points with missing depth values
  sub <- dplyr::filter(sub, !is.na(depth))
  # filter years based on covariate
  if (df$covariate[i] == "o2") {
    sub <- dplyr::filter(sub, year %in% seq(2010, 2015)) %>%
      dplyr::filter(!is.na(o2))
  } else {
    # temp observed for all years - but check for missing vals
    sub <- dplyr::filter(sub, !is.na(temp))
  }

  # rename variables to make code generic
  sub <- dplyr::rename(sub, enviro = as.character(df$covariate[i]))

  # make spde
  spde <- try(make_spde(
    x = sub$longitude, y = sub$latitude,
    n_knots = 250
  ), silent = TRUE)
  if (class(spde) != "try-error") {
    formula <- paste0("cpue_kg_km2 ~ -1")

    time_formula <- "~ -1"
    if (df$time_varying[i] == TRUE) {
      time_formula <- paste0(
        time_formula, " + ",
        "enviro", " + I(", "enviro", "^2)"
      )
      time_varying <- as.formula(time_formula)
      time <- "year"
    } else {
      formula <- paste0(
        formula, " + ",
        "enviro", " + I(", "enviro", "^2)"
      )
      time_varying <- NULL
      time <- "year"
    }
    formula <- paste0(formula, " + as.factor(year)")

    if (df$depth_effect[i] == TRUE) {
      formula <- paste0(formula, " + depth + I(depth^2)")
    }

    # fit model
    m <- try(sdmTMB(
      formula = as.formula(formula),
      time_varying = time_varying,
      spde = spde,
      time = time,
      family = tweedie(link = "log"),
      data = sub,
      anisotropy = TRUE,
      spatial_only = df$spatial_only[i],
      quadratic_roots = TRUE
    ), silent = TRUE)

    # sd_report <- summary(m$sd_report)
    # params <- as.data.frame(sd_report[grep("quadratic", row.names(sd_report)), ])

    if (class(m) != "try-error") {
      if (depth_filter == FALSE) {
        saveRDS(m, file = paste0("output/wc/depth_sensitivity/model_", i, ".rds"))
      } else {
        saveRDS(m, file = paste0("output/wc/depth_sensitivity/model_depthFilter_", i, ".rds"))
      }
      # sd_report <- summary(m$sd_report)
      # params <- as.data.frame(sd_report[grep("quadratic", row.names(sd_report)), ])
    }
  } # end try on spde
}
