library(dplyr)
library(pals)

set.seed(1234)

df <- readRDS("output/goa/models.RDS")

# parameters for back transforming standardized_temp
# temp_sd <- 1.983694
# temp_mean <- 6.800925

# gridded temp predictions
pred_temp <- readRDS("output/goa_pred_temp.rds")
pred_temp <- dplyr::rename(pred_temp, enviro = est)

# this block looks if there's any year where an anomalous temp value
# was predicted - because of variability in spatial coverage, etc and
# removes all data for that cell
pred_temp <- dplyr::group_by(pred_temp, lat_lon) %>%
  dplyr::mutate(n = length(which(abs(enviro) > 14))) %>%
  dplyr::filter(n == 0) %>%
  dplyr::select(-n)
pred_temp <- dplyr::filter(pred_temp, enviro > 0)

totarea = dplyr::group_by(pred_temp, year) %>% 
  dplyr::summarise(tot_area = sum(Area_km2))
totarea <- totarea$tot_area[1] # same in all years
#pred_temp_se <- readRDS("output/goa_pred_temp_uncertainty.rds")

# the sdms use a variable called 'depth' but it's really log(depth)
pred_temp$depth <- log(pred_temp$depth)
pred_temp$depth <- (pred_temp$depth - 4.847897) / 0.665218

for (i in c(1:nrow(df))) {
  if(file.exists(paste0("output/goa/model_", i, ".rds"))) {
    
  fit <- readRDS(file = paste0("output/goa/model_", i, ".rds"))

  # make predictions -- response not link space
  pred_df <- predict(fit, dplyr::filter(pred_temp,year %in% unique(fit$data$year))) # , type="response")

  # the GOA sampling is a little trickier than the other regions, because
  # we need to generate samples of the same area -- and cells are more 
  # unequally sized
  years <- unique(pred_df$year)
    for(ii in 1:length(years)) {
      # generate samples from this year that are equal in size to total area
      sub <- dplyr::filter(pred_df, year == years[ii])
      sampled_idx <- sample(1:nrow(sub), size = 1.2*length(unique(pred_df$lat_lon)), replace=T, prob = exp(sub$est))
      sampled_df <- sub[sampled_idx,]
      cutoff <- which(cumsum(sampled_df$Area_km2) > totarea)[1]
      sampled_df <- sampled_df[1:cutoff,]
      # summarize
      summarized_df <- dplyr::group_by(sampled_df, year) %>% 
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
      summarized_df$sci_name <- fit$data$scientific_name[1]
      summarized_df$species <- fit$data$species[1]
      summarized_df$depth <- df$depth_effect[i]
      if(ii == 1) {
        sampled_temp_year <- summarized_df
      } else {
        sampled_temp_year <- rbind(sampled_temp_year, summarized_df)
      }
    }
  
  if (i == 1) {
    all_temp <- sampled_temp_year
  } else {
    all_temp <- rbind(all_temp, sampled_temp_year)
  }
  
  }
}

saveRDS(all_temp, "output/temp_niche_goa.rds")
