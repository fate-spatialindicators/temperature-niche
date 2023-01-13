library(dplyr)
library(pals)
df <- readRDS("output/bc/models.RDS")

# parameters for back transforming standardized_temp
# temp_sd <- 1.983694
# temp_mean <- 6.800925

# gridded temp predictions
pred_temp <- readRDS("output/bc_pred_temp.rds")
pred_temp <- dplyr::rename(pred_temp, enviro = est)

# this block looks if there's any year where an anomalous temp value
# was predicted - because of variability in spatial coverage, etc and
# removes all data for that cell
pred_temp <- dplyr::group_by(pred_temp, lat_lon) %>%
  dplyr::mutate(n = length(which(abs(enviro) > 14))) %>%
  dplyr::filter(n == 0) %>%
  dplyr::select(-n)

#pred_temp$enviro <- (pred_temp$enviro - temp_mean) / temp_sd

pred_temp_se <- readRDS("output/bc_pred_temp_uncertainty.rds")

# dplyr::filter(pred_temp) %>%
#   ggplot(aes(longitude,latitude,fill=enviro)) +
#   geom_raster() + facet_wrap(~year)

# the sdms use a variable called 'depth' but it's really log(depth)
pred_temp$depth <- log(-pred_temp$depth)
pred_temp$depth <- (pred_temp$depth - 5.040364) / 0.6627643

# 2, 7, 30
for (i in c(31:nrow(df))) {
  if(file.exists(paste0("output/bc/model_", i, ".rds"))) {
    
  fit <- readRDS(file = paste0("output/bc/model_", i, ".rds"))

  # make predictions -- response not link space
  pred_df <- predict(fit, pred_temp) # , type="response")

  # now for each year, sample ~ 10000 temp values to get distribution
  sampled_temp <- sample(pred_df$enviro, size = 10000, prob = exp(pred_df$est))

  sampled_temp_year <- dplyr::group_by(pred_df, year) %>%
    do(sample_n(., size = 10000, replace = T, weight = exp(est)))
  sampled_temp_year$min <- min(sampled_temp)
  sampled_temp_year$max <- max(sampled_temp)
  sampled_temp_year$lo95 <- quantile(sampled_temp, 0.025)
  sampled_temp_year$hi95 <- quantile(sampled_temp, 0.975)
  sampled_temp_year$species <- df$species[i]
  sampled_temp_year$depth_effect <- df$depth_effect[i]

  # do empirical sampling
  empirical_temp_year <- dplyr::group_by(fit$data, year) %>%
    do(sample_n(., size = 10000, replace = T, weight = cpue_kg_km2))
  empirical_temp_year$species <- df$species[i]
  empirical_temp_year$depth_effect <- df$depth_effect[i]

  if (i == 1) {
    all_temp <- sampled_temp_year
    all_empirical <- empirical_temp_year
  } else {
    all_temp <- rbind(all_temp, sampled_temp_year)
    all_empirical <- rbind(all_empirical, empirical_temp_year)
  }
  # get empirical range of sampled temps
  # empirical_temp <- dplyr::group_by(fit$data, year) %>%
  #   dplyr::summarize(lo=quantile(enviro,0.025),
  #                    hi=quantile(enviro,0.975))

  # dplyr::group_by(sampled_temp_year,year) %>%
  #   dplyr::summarize(mean = mean(enviro),
  #                    lo=quantile(enviro,0.025),
  #                    hi=quantile(enviro,0.975)) %>%
  #   ggplot(aes(year,mean)) + geom_line() +
  #   geom_ribbon(aes(ymin=lo,ymax=hi)) +
  #   geom_line() +
  #   xlab("Year") +
  #   ylab("Temperature")
  }
}

saveRDS(all_temp, "output/temp_niche_bc.rds")
saveRDS(all_empirical, "output/empirical_temp_niche_bc.rds")
