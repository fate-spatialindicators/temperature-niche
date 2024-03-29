remotes::install_github("pbs-assess/sdmTMB", "885cee4")
library(dplyr)
library(pals)
library(sp)
library(sdmTMB)
set.seed(1234)

cow <- readRDS("survey_data/joined_nwfsc_data.rds")
bc <- readRDS("survey_data/bc_data_2021.rds")
bc <- dplyr::select(bc, -start_time, -count, -sensor_name) %>%
  dplyr::rename(temp = temperature)
goa <- readRDS("survey_data/joined_goa_data.rds")

cow$region <- "COW"
bc$region <- "BC"
goa$region <- "GOA"

dat <- rbind(dplyr::select(bc, year, survey, species, scientific_name, 
                           latitude_dd, longitude_dd, depth, temp, 
                           cpue_kg_km2, region),
             dplyr::select(cow, year, survey, species, scientific_name, 
                           latitude_dd, longitude_dd, depth, temp, 
                           cpue_kg_km2, region),
             dplyr::select(goa, year, survey, species, scientific_name, 
                           latitude_dd, longitude_dd, depth, temp, 
                           cpue_kg_km2, region))
dat$species <- tolower(dat$species)
dat$species[which(dat$species%in%c("spiny dogfish","pacific spiny dogfish"))] = "north pacific spiny dogfish"

# filter common years
dat <- dplyr::filter(dat, year>=2003)

df_wc <- readRDS("output/wc/models.RDS")
df_goa <- readRDS("output/goa/models.RDS")
df_bc <- readRDS("output/bc/models.RDS")
# filter only models that have depth
df_wc <- dplyr::mutate(df_wc, id = 1:nrow(df_wc)) %>% 
  dplyr::filter(depth_effect==TRUE)
df_goa <- dplyr::mutate(df_goa, id = 1:nrow(df_goa)) %>% 
  dplyr::filter(depth_effect==TRUE)
df_bc <- dplyr::mutate(df_bc, id = 1:nrow(df_bc)) %>% 
  dplyr::filter(depth_effect==TRUE)
# 'pacific spiny dogfish' in wc data
# 'spiny dogfish' in goa data
# 'north pacific spiny dogfish' in bc data
df_wc$species = as.character(df_wc$species)
df_bc$species = tolower(as.character(df_bc$species))
df_goa$species = tolower(as.character(df_goa$species))
df_wc$species[which(df_wc$species=="pacific spiny dogfish")] = "north pacific spiny dogfish"
df_goa$species[which(df_goa$species=="spiny dogfish")] = "north pacific spiny dogfish"

# species table
species_table <- read.csv("species_table.csv")
# species_table = dplyr::mutate(species_table, 
#                               GOA = ifelse(GOA=="x",1,0),
#                               BC = ifelse(BC=="x",1,0),
#                               WC = ifelse(WC=="x",1,0),
#                               n_regions = GOA + BC + WC) %>%
#   dplyr::filter(n_regions > 1)
#  
species_table <- dplyr::filter(species_table, n_region>1)

for (i in 1:nrow(species_table)) {
  this_species = species_table$species[i]
  sub <- dplyr::filter(dat, species == this_species, !is.na(depth))
  sub <- add_utm_columns(sub, ll_names = c("longitude_dd", "latitude_dd"))
  
  sub$enviro <- sub$temp
  sub$enviro2 <- sub$enviro * sub$enviro
  sub$logdepth <- scale(log(sub$depth))
  sub$logdepth2 <- sub$logdepth * sub$logdepth
  sub <- dplyr::filter(sub, !is.na(enviro), !is.na(depth))

  sub$scaled_enviro <- scale(sub$enviro)
  sub$scaled_enviro2 <- sub$scaled_enviro * sub$scaled_enviro
  
  bnd <- INLA::inla.nonconvex.hull(cbind(sub$X, sub$Y), 
                                   convex = -0.05)
  inla_mesh <- INLA::inla.mesh.2d(
    boundary = bnd,
    max.edge = c(150, 1000),
    offset = -0.1, # default -0.1
    cutoff = 50,
    min.angle = 5 # default 21
  )
  spde <- make_mesh(sub, c("X", "Y"), mesh = inla_mesh)
  
  priors <- sdmTMBpriors(
    matern_s = pc_matern(
      range_gt = 50, range_prob = 0.05, #A value one expects the range is greater than with 1 - range_prob probability.
      sigma_lt = 25, sigma_prob = 0.05 #A value one expects the marginal SD (sigma_O or sigma_E internally) is less than with 1 - sigma_prob probability.
    ),
    matern_st = pc_matern(
      range_gt = 50, range_prob = 0.05,
      sigma_lt = 25, sigma_prob = 0.05
    ),
    ar1_rho = normal(0.7,0.1),
    tweedie_p = normal(1.5,0.2)
  )
  # refactor to avoid identifiability errors
  sub$region <- as.factor(as.character(sub$region))

  # Model 1
  formula = "cpue_kg_km2 ~ 1 + logdepth + logdepth2"

  fit <- sdmTMB(
    formula = as.formula(formula),
    mesh = spde,
    time = "year",
    family = tweedie(link = "log"),
    data = sub,
    #priors = priors,
    share_range = TRUE,
    spatial = "on",
    spatiotemporal = "rw",
    control = sdmTMBcontrol(normalize = TRUE,
                            multiphase = TRUE, 
                            newton_loops = 1),
    extra_time = (2003:2021)[which(2003:2021 %in% unique(sub$year) == FALSE)]
  )
  
  saveRDS(fit, file = paste0("output/all/", this_species, "_model1.rds"))
  
  # Model 2
  fit <- sdmTMB(
    cpue_kg_km2 ~ 1 + scaled_enviro + scaled_enviro2 + logdepth + logdepth2,
    mesh = spde,
    time = "year",
    family = tweedie(link = "log"),
    data = sub,
    #priors = priors,
    share_range = TRUE,
    spatial = "on",
    spatiotemporal = "rw",
    control = sdmTMBcontrol(normalize = TRUE,
                            multiphase = TRUE),
    extra_time = (2003:2021)[which(2003:2021 %in% unique(sub$year) == FALSE)]
  )
  saveRDS(fit, file = paste0("output/all/", this_species, "_model2.rds"))
  
  # Model 3
  fit <- sdmTMB(
    cpue_kg_km2 ~ 1 + scaled_enviro + scaled_enviro2 + scaled_enviro:region + scaled_enviro2:region + logdepth + logdepth2,
    mesh = spde,
    time = "year",
    family = tweedie(link = "log"),
    data = sub,
    #priors = priors,
    share_range = TRUE,
    spatial = "on",
    spatiotemporal = "rw",
    control = sdmTMBcontrol(normalize = TRUE,
                            multiphase = TRUE),
    extra_time = (2003:2021)[which(2003:2021 %in% unique(sub$year) == FALSE)]
  )
  saveRDS(fit, file = paste0("output/all/", this_species, "_model3.rds"))

  # Model 4:Constant temp, depth region interaction
  fit <- sdmTMB(
    cpue_kg_km2 ~ 1 + scaled_enviro + scaled_enviro2 + region:logdepth + region:logdepth2,
    mesh = spde,
    time = "year",
    family = tweedie(link = "log"),
    data = sub,
    #priors = priors,
    share_range = TRUE,
    spatial = "on",
    spatiotemporal = "rw",
    control = sdmTMBcontrol(normalize = TRUE,
                            multiphase = TRUE),
    extra_time = (2003:2021)[which(2003:2021 %in% unique(sub$year) == FALSE)]
  )
  saveRDS(fit, file = paste0("output/all/", this_species, "_model4.rds"))
  
  # Model 5:variable enviro and depth interactions
  fit <- sdmTMB(
    cpue_kg_km2 ~ 1 + region:scaled_enviro + region:scaled_enviro2 + region:logdepth + region:logdepth2,
    mesh = spde,
    time = "year",
    family = tweedie(link = "log"),
    data = sub,
    #priors = priors,
    share_range = TRUE,
    spatial = "on",
    spatiotemporal = "rw",
    control = sdmTMBcontrol(normalize = TRUE,
                            multiphase = TRUE),
    extra_time = (2003:2021)[which(2003:2021 %in% unique(sub$year) == FALSE)]
  )
  saveRDS(fit, file = paste0("output/all/", this_species, "_model5.rds"))
}
  
aic_table = matrix(NA, nrow(species_table), 5)
for(i in 6:nrow(species_table)) {
  for(j in 1:5) {
    this_species = species_table$species[i]
    fit <- readRDS(file = paste0("output/all/", this_species, "_model",j,".rds"))
    s <- sanity(fit, silent=TRUE)
    if(s$hessian_ok + s$eigen_values_ok + s$nlminb_ok == 3) aic_table[i,j] = AIC(fit)
  }
}
write.csv(aic_table, "aic_table.csv")
write.csv(cbind(species_table, aic_table), "combined_table.csv")

combined <- read.csv("combined_table.csv")

for(i in 1:nrow(combined)) combined[i,8:12] <- combined[i,8:12] - min(combined[i,8:12],na.rm=T)
write.csv(combined, "combined_table.csv")

spp <- readRDS("output/spp_for_brms.rds")
# filter out the 30 spp used in paper
spp <- spp[-which(spp == "Pacific ocean perch")]
spp <- c(spp, "Longspine thornyhead")
combined <- combined[which(combined$species %in% tolower(spp)),]
write.csv(combined, "combined_table.csv")
