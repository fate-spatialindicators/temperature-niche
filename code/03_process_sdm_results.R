library(ggplot2)
library(dplyr)
library(sdmTMB)

region <- c("goa", "wc", "bc")[2]
df <- readRDS(paste0("output/", region, "/models.RDS"))

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
  fname <- paste0("output/", region, "/model_", i, ".rds")
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
      if (i == 1) {
        pred_all <- pred
      } else {
        pred_all <- rbind(pred_all, pred)
      }
    }
  }
}

write.csv(pred_all, file = paste0("output/", region, "_output_covar_effects.csv"))
# save results
write.csv(df, file = paste0("output/", region, "_output.csv"))
# saveRDS(df,file=paste0("output/",region,"_output.rds"))
# 
# df <- read.csv(
#   file = paste0("output/", region, "_output.csv"),
#   stringsAsFactors = FALSE
# )
# 
# pred_all <- read.csv(paste0("output/", region, "_output_covar_effects.csv"))
# ggplot(pred_all, aes(enviro, est_non_rf, col = quadratic, group = quadratic)) +
#   geom_line() +
#   facet_wrap(~species, scale = "free_y") +
#   theme_bw() +
#   xlab("Standardized temperature") +
#   ylab("Estimated effect") +
#   theme(strip.background = element_rect(fill = "white"))
# 
# pdf(paste0("plots/", region, "-temp_range.pdf"))
# level_order <- dplyr::filter(
#   df, !is.na(range), covariate == "temp",
#   depth_effect == TRUE, range_se < 5
# ) %>%
#   dplyr::arrange(range) %>%
#   select(species)
# dplyr::filter(
#   df, !is.na(range), covariate == "temp",
#   range_se < 5, depth_effect == TRUE
# ) %>%
#   ggplot(aes(factor(species, level = level_order$species), range)) +
#   geom_pointrange(aes(
#     ymin = range - 2 * range_se,
#     ymax = range + 2 * range_se
#   )) +
#   coord_flip() +
#   xlab("Species") +
#   ylab("Range") +
#   ggtitle(paste0("Temperature range - ", region, " survey"))
# dev.off()
# 
# pdf(paste0("plots/", region, "-temp_range2.pdf"))
# level_order <- dplyr::filter(
#   df, !is.na(range), covariate == "temp",
#   depth_effect == TRUE, range_se < 1
# ) %>%
#   dplyr::arrange(range) %>%
#   select(species)
# dplyr::filter(
#   df, !is.na(range), covariate == "temp", range_se < 1,
#   species %in% level_order$species
# ) %>%
#   ggplot(aes(factor(species, level = level_order$species), range, col = depth_effect)) +
#   geom_pointrange(aes(
#     ymin = range - 2 * range_se,
#     ymax = range + 2 * range_se
#   )) +
#   coord_flip() +
#   xlab("Species") +
#   ylab("Range") +
#   ggtitle(paste0("Temperature range - ", region, " survey"))
# dev.off()
# 
# pdf(paste0("plots/", region, "-temp_low.pdf"))
# level_order <- dplyr::filter(
#   df, !is.na(lo), covariate == "temp",
#   depth_effect == TRUE, lo_se < 5
# ) %>%
#   dplyr::arrange(lo) %>%
#   select(species)
# dplyr::filter(df, !is.na(lo), covariate == "temp", depth_effect == TRUE, lo_se < 5) %>%
#   ggplot(aes(factor(species, level = level_order$species), lo)) +
#   geom_pointrange(aes(
#     ymin = lo - 2 * lo_se,
#     ymax = lo + 2 * lo_se
#   ), col = "darkblue") +
#   coord_flip() +
#   xlab("Species") +
#   ylab("Range") +
#   ggtitle(paste0("Temperature low bound - ", region, " survey"))
# dev.off()
# 
# pdf(paste0("plots/", region, "-temp_hi.pdf"))
# level_order <- dplyr::filter(
#   df, !is.na(hi), covariate == "temp",
#   depth_effect == TRUE, hi_se < 5
# ) %>%
#   dplyr::arrange(hi) %>%
#   select(species)
# dplyr::filter(df, !is.na(hi), covariate == "temp", depth_effect == TRUE, hi_se < 5) %>%
#   ggplot(aes(factor(species, level = level_order$species), hi)) +
#   geom_pointrange(aes(
#     ymin = hi - 2 * hi_se,
#     ymax = hi + 2 * hi_se
#   ), col = "darkblue") +
#   coord_flip() +
#   xlab("Species") +
#   ylab("Range") +
#   ggtitle(paste0("Temperature upper bound - ", region, " survey"))
# dev.off()
# 
# pdf(paste0("plots/", region, "-temp_reduction.pdf"))
# level_order <- dplyr::filter(
#   df, !is.na(reduction), covariate == "temp",
#   depth_effect == TRUE, reduction_se < 5
# ) %>%
#   dplyr::arrange(reduction) %>%
#   select(species)
# dplyr::filter(df, !is.na(reduction), covariate == "temp", depth_effect == TRUE, reduction_se < 5) %>%
#   ggplot(aes(factor(species, level = level_order$species), reduction)) +
#   geom_pointrange(aes(
#     ymin = reduction - 2 * reduction_se,
#     ymax = reduction + 2 * reduction_se
#   ), col = "darkblue") +
#   coord_flip() +
#   xlab("Species") +
#   ylab("Range") +
#   ggtitle(paste0("Reduction from peak - ", region, " survey"))
# dev.off()
# 
# pdf(paste0("plots/", region, "-temp_reduction2.pdf"))
# level_order <- dplyr::filter(
#   df, !is.na(reduction), covariate == "temp",
#   depth_effect == TRUE, reduction_se < 5
# ) %>%
#   dplyr::arrange(reduction) %>%
#   select(species)
# dplyr::filter(
#   df, !is.na(reduction), covariate == "temp", reduction_se < 5,
#   species %in% level_order$species
# ) %>%
#   ggplot(aes(factor(species, level = level_order$species), reduction, col = depth_effect)) +
#   geom_pointrange(aes(
#     ymin = reduction - 2 * reduction_se,
#     ymax = reduction + 2 * reduction_se
#   )) +
#   coord_flip() +
#   xlab("Species") +
#   ylab("Reduction") +
#   ggtitle(paste0("Temperature range - ", region, " survey"))
# dev.off()
# 
# 
# 
# pdf(paste0("plots/", region, "-temp_m.pdf"))
# 
# 
# dev.off()
