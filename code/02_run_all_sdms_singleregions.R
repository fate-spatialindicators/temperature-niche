library(dplyr)
library(pals)
library(sp)

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
species_table <- dplyr::filter(species_table, n_region==1)
for (i in 1:nrow(species_table)) {
  this_species = species_table$species[i]
  sub <- dplyr::filter(dat, species == this_species, !is.na(depth))
  sub <- add_utm_columns(sub, ll_names = c("longitude_dd", "latitude_dd"))
  
  sub$enviro <- sub$temp
  sub$enviro2 <- sub$enviro * sub$enviro
  sub$logdepth <- scale(log(sub$depth))
  sub$logdepth2 <- sub$logdepth * sub$logdepth
  sub <- dplyr::filter(sub, !is.na(enviro), !is.na(depth))

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

  fit = list()
  
  # Model 1
  formula = "cpue_kg_km2 ~ -1 + logdepth + logdepth2"
  if(length(unique(sub$region)) > 1) formula <- paste(formula, "+ region")

  fit[[1]] <- try(sdmTMB(
    formula = as.formula(formula),
    mesh = spde,
    time = "year",
    family = tweedie(link = "log"),
    data = sub,
    priors = priors,
    share_range = FALSE,
    spatial = "on",
    spatiotemporal = "rw",
    control = sdmTMBcontrol(quadratic_roots = FALSE, 
                            normalize = TRUE,
                            multiphase = TRUE,
                            newton_loops = 1),
    extra_time = (1991:2021)[which(1991:2021 %in% unique(sub$year) == FALSE)]
  ), silent = TRUE)
  
  # Model 2
  new_formula <- "cpue_kg_km2 ~ -1 + enviro + enviro2 + logdepth + logdepth2"
  if(length(unique(sub$region)) > 1) new_formula <- paste(new_formula, "+ region")
  
  fit[[2]] <- try(update(fit[[1]], formula = as.formula(new_formula), share_range=FALSE), silent = TRUE)
  fit[[3]] <- try(update(fit[[1]], formula = as.formula(new_formula), share_range=TRUE), silent = TRUE)
  fit[[4]] <- try(update(fit[[1]], share_range=TRUE), silent = TRUE)
  
  #new_formula <- "cpue_kg_km2 ~ -1 + enviro + enviro2 + logdepth + logdepth2"
  #if(length(unique(sub$region)) > 1) new_formula <- paste(new_formula, "+ region")
  #fit[[3]] <- try(update(fit[[1]], formula = as.formula(new_formula),spatiotemporal = "rw", share_range=FALSE), silent = TRUE)
  
  if(length(unique(sub$region)) > 1) {
    # Model 3:Add region:enviornment interaction
    # only run this for > 1 region, otherwise it's identical to model 1
    new_formula <- "cpue_kg_km2 ~ -1 + enviro + enviro2 + enviro*region + enviro2*region + logdepth + logdepth2"
    fit[[5]] <- try(update(fit[[1]], formula = as.formula(new_formula)), silent = TRUE)
    
    # Model 4:Constant temp, depth region interaction
    # only run this for > 1 region, otherwise it's identical to model 1
    new_formula <- "cpue_kg_km2 ~ -1 + enviro + enviro2 + region*logdepth + region*logdepth2"
    fit[[6]] <- try(update(fit[[1]], formula = as.formula(new_formula)), silent = TRUE)
  }
  # save list of fitted models
  saveRDS(fit, file = paste0("output/all/", this_species, ".rds"))
#toc()
}
  

species_table$model_1 = NA
species_table$model_2 = NA
species_table$model_3 = NA
for (i in 1:nrow(species_table)) {
  
 fit = readRDS(paste0("output/all/", species_table$species[i], ".rds")) 
 s = sanity(fit[[1]])
 species_table$model_1[i] = s$hessian_ok + s$eigen_values_ok + s$nlminb_ok
 if(species_table$model_1[i] == 2) {
   species_table$model_1[i] = AIC(fit[[1]]) 
 } else {
   species_table$model_1[i] = NA
 }
   
 s = sanity(fit[[2]])
 species_table$model_2[i] = s$hessian_ok + s$eigen_values_ok + s$nlminb_ok
 if(species_table$model_2[i] == 3) {
   species_table$model_2[i] = AIC(fit[[2]]) 
 } else {
   species_table$model_2[i] = NA
 }
 
 s = sanity(fit[[3]])
 species_table$model_3[i] = s$hessian_ok + s$eigen_values_ok + s$nlminb_ok
 if(species_table$model_3[i] == 3) {
   species_table$model_3[i] = AIC(fit[[3]]) 
 } else {
   species_table$model_3[i] = NA
 }
 
}
species_table$best_model <- apply(species_table[,7:8],1,which.min) + 1
saveRDS(species_table,"species_table_singleregion.rds")