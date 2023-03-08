library(ggplot2)
library(dplyr)
library(sdmTMB)

region <- c("goa", "wc", "bc")[3]
df <- readRDS(paste0("output/", region, "/models.RDS"))
pred_all <- NULL
presence_only = TRUE
presence_str = ifelse(presence_only, "_presence","")

df$lo <- NA
df$lo_se <- NA
df$hi <- NA
df$hi_se <- NA
df$range <- NA
df$range_se <- NA
df$peak <- NA
df$peak_se <- NA
df$reduction <- NA
df$reduction_se <- NA
df$converged <- 0
df$b_env <- NA
df$b_env_se <- NA
df$b_env2 <- NA
df$b_env2_se <- NA

# Create a data frame just to project the effect of temperature on
m <- readRDS(paste0("output/", region, "/model_", 1, ".rds"))
ref_year <- 2013
newdf <- m$data
newdf$latitude <- mean(newdf$latitude) # make prediction for avg location
newdf$longitude <- mean(newdf$longitude)
newdf$depth <- newdf$depth * 0.0
unique_years <- unique(newdf$year)
newdf <- newdf[1:(61 * length(unique_years)), ]
newdf$enviro <- rep(seq(-3, 3, by = 0.1), length(unique_years))
newdf$year <- sort(rep(unique_years, 61))

for (i in 1:nrow(df)) {
  print(i)
  fname <- paste0("output/", region, "/model_", i,presence_str, ".rds")
  if (file.exists(fname)) {
    m <- readRDS(fname)
    sd_report <- summary(m$sd_report)

    params <- as.data.frame(sd_report[grep("quadratic", row.names(sd_report)), ])
    df$lo[i] <- params["quadratic_low", "Estimate"]
    df$lo_se[i] <- params["quadratic_low", "Std. Error"]
    df$hi[i] <- params["quadratic_hi", "Estimate"]
    df$hi_se[i] <- params["quadratic_hi", "Std. Error"]
    df$range[i] <- params["quadratic_range", "Estimate"]
    df$range_se[i] <- params["quadratic_range", "Std. Error"]
    df$peak[i] <- params["quadratic_peak", "Estimate"]
    df$peak_se[i] <- params["quadratic_peak", "Std. Error"]
    df$reduction[i] <- params["quadratic_reduction", "Estimate"]
    df$reduction_se[i] <- params["quadratic_reduction", "Std. Error"]
    
    if (length(which(is.na(m$sd_report$sd))) == 0) df$converged[i] <- 1

    if (df$converged[i] == 1) {
      tidy_pars <- tidy(m)
      df$b_env[i] <- tidy_pars$estimate[1]
      df$b_env_se[i] <- tidy_pars$std.error[1]
      df$b_env2[i] <- tidy_pars$estimate[2]
      df$b_env2_se[i] <- tidy_pars$std.error[2]
      # now do predictions for this model and the gam at average depth
      # if(df$spline[i]==FALSE) {
      #   newdf$enviro = scale(newdf$enviro)*1.775293
      # }
      pred <- predict(m, newdata = dplyr::filter(newdf, year %in% unique(m$data$year)))
      pred <- dplyr::select(
        pred, -epsilon_st,
        -omega_s, -est_rf, -est
      )
      pred <- dplyr::filter(pred, year == ref_year)
      pred$species <- as.character(df$species[i])
      pred$quadratic <- df$quadratic[i]
      pred$model <- i
      temp_effect <- pred$est_non_rf
      
      if("surface_temperature" %in% names(pred)) pred <- dplyr::select(pred, -surface_temperature)
      if("date" %in% names(pred)) pred <- dplyr::select(pred, -date)
      if("day" %in% names(pred)) pred <- dplyr::select(pred, -day)
      if("month" %in% names(pred)) pred <- dplyr::select(pred, -month)
      if("species_code" %in% names(pred)) pred <- dplyr::select(pred, -species_code)
      
      if (i == 1) {
        pred_all <- pred
      } else {
        pred_all <- rbind(pred_all, pred)
      }
    }
  }
}

write.csv(pred_all, file = paste0("output/", region,presence_str, "_output_covar_effects.csv"))
# save results
write.csv(df, file = paste0("output/", region,presence_str, "_output.csv"))
