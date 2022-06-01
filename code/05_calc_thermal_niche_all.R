library(dplyr)
library(pals)
library(sp)

df_wc <- readRDS("output/wc/models.RDS")
df_goa <- readRDS("output/goa/models.RDS")
# filter only models that have depth
df_wc <- dplyr::mutate(df_wc, id = 1:nrow(df_wc)) %>% 
  dplyr::filter(depth_effect==TRUE)
df_goa <- dplyr::mutate(df_goa, id = 1:nrow(df_goa)) %>% 
  dplyr::filter(depth_effect==TRUE)
# 'pacific spiny dogfish' in wc data
# 'spiny dogfish' in goa data
# 'north pacific spiny dogfish' in bc data
df_wc$species = as.character(df_wc$species)
df_goa$species = tolower(as.character(df_goa$species))
df_wc$species[which(df_wc$species=="pacific spiny dogfish")] = "north pacific spiny dogfish"
df_goa$species[which(df_goa$species=="spiny dogfish")] = "north pacific spiny dogfish"

# overlap
df_goa <- df_goa[which(df_goa$species%in% df_wc$species),]
df_wc <- df_wc[which(df_wc$species%in% df_goa$species),]
species = df_goa$species

# gridded temp predictions from GOA
pred_temp <- readRDS("output/goa_pred_temp.rds")
pred_temp <- dplyr::rename(pred_temp, enviro = est)
# bring in cell areas
goa_area <- read.csv("grid_data/grid_goa.csv")
pred_temp_goa <- left_join(pred_temp, goa_area[,c("Id","Area_km2")])
pred_temp_goa$depth <- (pred_temp_goa$logdepth - 4.847777) / 0.6629802
# TODO: fix the area standardization
pred_temp_goa <- dplyr::filter(pred_temp_goa, Area_km2 > 9)

# gridded temp predictions from WC
pred_temp_wc <- readRDS("output/wc_pred_temp.rds")
pred_temp_wc <- dplyr::rename(pred_temp_wc, enviro = est)
pred_temp_wc$Area_km2 <- 10.28971 # area size of raster cells
pred_temp_wc$depth <- log(-pred_temp_wc$depth)
pred_temp_wc$depth <- (pred_temp_wc$depth - 5.614747) / 0.896874

for (i in 1:length(species)) {
  
  # WC
  spp <- df_wc$id[which(df_wc$species==species[i])]
  fit <- readRDS(file = paste0("output/wc/model_", spp, ".rds"))
  # make predictions -- response not link space
  pred_df_wc <- predict(fit, pred_temp_wc) # , type="response")

  # GOA
  spp <- df_goa$id[which(df_goa$species==species[i])]
  fit <- readRDS(file = paste0("output/goa/model_", spp, ".rds"))
  # make predictions -- response not link space
  pred_df_goa <- predict(fit, pred_temp_goa) # , type="response")
  
  pred_df <- rbind(pred_df_goa[,c("year","est","enviro")], pred_df_wc[,c("year","est","enviro")])
  
  common_years = unique(pred_df_goa$year)[which(unique(pred_df_goa$year) %in% unique(pred_df_wc$year))]
  pred_df = dplyr::filter(pred_df, year %in% common_years)
  # now for each year, sample ~ 10000 temp values to get distribution
  sampled_temp <- sample(pred_df$enviro, size = 10000, prob = exp(pred_df$est))

  sampled_temp_year <- dplyr::group_by(pred_df, year) %>%
    do(sample_n(., size = 10000, replace = T, weight = exp(est)))
  sampled_temp_year$min <- min(sampled_temp)
  sampled_temp_year$max <- max(sampled_temp)
  sampled_temp_year$lo95 <- quantile(sampled_temp, 0.025)
  sampled_temp_year$hi95 <- quantile(sampled_temp, 0.975)
  sampled_temp_year$species <- species[i]
  #sampled_temp_year$depth_effect <- df$depth_effect[i]

  if (i == 1) {
    all_temp <- sampled_temp_year
  } else {
    all_temp <- rbind(all_temp, sampled_temp_year)
  }

}

saveRDS(all_temp, "output/temp_niche_combined.rds")
