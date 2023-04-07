# devtools::install_github("pbs-assess/sdmTMB", "quadratic-roots")
library(sdmTMB)
library(dplyr)
library(sp)

dat <- readRDS("survey_data/bc_data_2021.rds")
dat <- dplyr::select(dat, -start_time, -count, -sensor_name) %>%
  dplyr::rename(temp = temperature)

# base model configuration
model_config <- ""

# can be changed to put a new set of make a sub folder by the name used here e.g.
# model_config <- "survey_effect/"
# model_config <- "cutoff_12km/"
# model_config <- "weighted_depth/“

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

# filter out species with < 50 samples in a year
summary <- read.csv("output/summary_statistics_bc.csv")
summary <- dplyr::filter(summary, mean_n >= 50)
# 50 min_n used for other areas, but given alternating yr samples, I think mean works better for BC
# I also haven't trimmed out 2003, 2004 sampling that was just in QCS, should we?
dat <- dplyr::filter(dat, species %in% summary$species)

# create grid of spp. Run each 2x, with and without
# quadratic roots. without quadratic, use a smooth function instead
df <- expand.grid(
  "species" = unique(dat$species),
  spatial_only = c(FALSE),
  depth_effect = c(TRUE),
  time_varying = c(FALSE),
  quadratic = c(TRUE),
  covariate = c("temp")
)
saveRDS(df, "output/bc/models.RDS")

presence_only = FALSE
presence_str = ifelse(presence_only, "_presence","")

for (i in 1:nrow(df)) {
  sub <- dplyr::filter(dat, species == df$species[i])

  sub$suvey <- factor(sub$survey, levels = c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"))

  # rename variables to make code generic
  sub <- dplyr::rename(sub, enviro = as.character(df$covariate[i]))

  # drop points with missing depth or enviro values
  sub <- dplyr::filter(sub, !is.na(depth), !is.na(enviro))

  # save means and sd used in scaling of covariates for ease of back transformation for figures?
  # centre depth to full BC surveys' mean sampled depth
  # sub$depth_mean = mean(log(sub$depth))

  # or centre depth based on each species weighted mean depth occupied as calculated in the summary csv
  # sub$depth_mean = log(summary$weighted_depth[summary$species == as.character(df$species[i])])

  # sub$depth_sd = sd(log(sub$depth))

  # sub$enviro_mean = mean(sub$enviro)
  # sub$enviro_sd = sd(sub$enviro)

  # rescale variables
  sub$depth <- as.numeric(scale(log(sub$depth))) # scaled globally
  # sub$depth = as.numeric((log(sub$depth)-sub$depth_mean)/sub$depth_sd)
  #sub$enviro = as.numeric((scale(sub$enviro)))
  # sub$enviro = as.numeric((sub$enviro-sub$enviro_mean)/sub$enviro_sd)
  sub$presence <- ifelse(sub$cpue_kg_km2 > 0, 1, 0)
  # browser()
  # make spde
  spde <- try(make_mesh(sub, c("longitude", "latitude"),
    cutoff = 20
  ), silent = TRUE) # may want to make smaller, but keeping same as WC for now

  # browser()
  if (class(spde) != "try-error") {
    formula <- paste0("cpue_kg_km2 ~ -1")
    if(presence_only) formula <- paste0("presence ~ -1")
    time_formula <- "~ -1"

    if (df$quadratic[i] == TRUE) {
      formula <- paste0(
        formula, " + ",
        "enviro", " + I(", "enviro", "^2)"
      )
      time_varying <- NULL
      time <- "year"
    } else {
      formula <- paste0(
        formula, " + ",
        "s(enviro,k=3)"
      )
      time_varying <- NULL
      time <- "year"
    }

    formula <- paste0(formula, " + as.factor(year)") # + survey # I experimented with adding survey and it

    if (df$depth_effect[i] == TRUE) {
      formula <- paste0(formula, " + depth + I(depth^2)") # formula = paste0(formula, " + depth + I(depth^2)")
    }

    # use PC prior for matern model
    priors <- sdmTMBpriors(
      matern_s = pc_matern(
        range_gt = 5, range_prob = 0.05,
        sigma_lt = 25, sigma_prob = 0.05
      )
    )
    # fit model
    if(presence_only==TRUE) {
      m <- try(sdmTMB(
        formula = as.formula(formula),
        time_varying = time_varying,
        mesh = spde,
        time = time,
        family = binomial(),
        data = sub,
        priors = priors,
        spatial = "on",
        spatiotemporal = "iid",
        control = sdmTMBcontrol(quadratic_roots = df$quadratic[i])
      ), silent = TRUE) 
    } else {
      m <- try(sdmTMB(
        formula = as.formula(formula),
        time_varying = time_varying,
        mesh = spde,
        time = time,
        family = tweedie(link = "log"),
        data = sub,
        priors = priors,
        spatial = "on",
        spatiotemporal = "iid",
        control = sdmTMBcontrol(quadratic_roots = df$quadratic[i])
      ), silent = TRUE) 
    }

    if (class(m) != "try-error") {
      saveRDS(m, file = paste0("output/bc/model_", i,presence_str, ".rds"))
    }
  } # end try on spde
}
