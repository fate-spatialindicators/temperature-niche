library(dplyr)
library(pals)

set.seed(1234)

df <- readRDS("output/wc/models.RDS")

# parameters for back transforming standardized_temp
# temp_sd <- 1.983694
# temp_mean <- 6.800925

# gridded temp predictions
pred_temp <- readRDS("output/wc_pred_temp.rds")
pred_temp <- dplyr::rename(pred_temp, enviro = est)

# this block looks if there's any year where an anomalous temp value
# was predicted - because of variability in spatial coverage, etc and
# removes all data for that cell
pred_temp <- dplyr::group_by(pred_temp, lat_lon) %>%
  dplyr::mutate(n = length(which(abs(enviro) > 14))) %>%
  dplyr::filter(n == 0) %>%
  dplyr::select(-n)
pred_temp <- dplyr::filter(pred_temp, enviro > 0)

# the sdms use a variable called 'depth' but it's really log(depth)
pred_temp$depth <- log(-pred_temp$depth)
pred_temp$depth <- (pred_temp$depth - 5.606277) / 0.8947727

for (i in c(1:nrow(df))) {
  if(file.exists(paste0("output/wc/model_", i, ".rds"))) {
    
  # temp model
  fit <- readRDS(file = paste0("output/wc/model_", i, ".rds"))

  # make predictions -- response not link space
  pred_df <- predict(fit, pred_temp) # , type="response")

  # now for each year, sample temp values to get distribution
  # we are sampling temperature, weighted by estimated density. 
  # cell sizes basically uniform -- so in all cases, we want to sample
  # areas that are the same size as the prediction grid.
  #tot_area <- 1028.413 * length(unique(pred_df$lat_lon))
  # 1028.413 = mean(availablecells$Hectares) in nwfscSurvey
  
  
  #sampled_temp <- sample(pred_df$enviro, size = 10000, prob = exp(pred_df$est))
  sampled_temp_year <- dplyr::group_by(pred_df, year) %>%
     do(sample_n(., size = length(unique(pred_df$lat_lon)), replace = T, weight = exp(est)))
  sampled_temp_year <- 
    dplyr::select(sampled_temp_year, year, enviro, logdepth, logdepth_orig) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(min_enviro = min(enviro),
                     max_enviro = max(enviro),
                     mean_enviro = mean(enviro),
                     lo10_enviro = quantile(enviro,0.05),
                     lo20_enviro = quantile(enviro,0.1),
                     lo30_enviro = quantile(enviro,0.15),
                     lo40_enviro = quantile(enviro,0.2),
                     lo50_enviro = quantile(enviro,0.25),
                     hi10_enviro = quantile(enviro,0.95),
                     hi20_enviro = quantile(enviro,0.9),
                     hi30_enviro = quantile(enviro,0.85),
                     hi40_enviro = quantile(enviro,0.8),
                     hi50_enviro = quantile(enviro,0.75),
                     min_depth = min(logdepth),
                     max_depth = max(logdepth),
                     mean_depth = mean(logdepth),
                     lo_depth = quantile(logdepth,0.025),
                     hi_depth = quantile(logdepth,0.975))
  
  # sampled_temp_year$min <- min(sampled_temp)
  # sampled_temp_year$max <- max(sampled_temp)
  # sampled_temp_year$lo95 <- quantile(sampled_temp, 0.025)
  # sampled_temp_year$hi95 <- quantile(sampled_temp, 0.975)
  sampled_temp_year$sci_name <- fit$data$scientific_name[1]
  sampled_temp_year$species <- fit$data$species[1]
  sampled_temp_year$depth <- df$depth_effect[i]
  # do empirical sampling
  # empirical_temp_year <- dplyr::group_by(fit$data, year) %>%
  #   do(sample_n(., size = 10000, replace = T, weight = cpue_kg_km2))
  # empirical_temp_year$species <- df$species[i]
  # empirical_temp_year$depth_effect <- df$depth_effect[i]

  if (i == 1) {
    all_temp <- sampled_temp_year
    #all_empirical <- empirical_temp_year
  } else {
    all_temp <- rbind(all_temp, sampled_temp_year)
    #all_empirical <- rbind(all_empirical, empirical_temp_year)
  }
  
  }
}

saveRDS(all_temp, "output/temp_niche_wc.rds")
#saveRDS(all_empirical, "output/empirical_temp_niche_wc.rds")
