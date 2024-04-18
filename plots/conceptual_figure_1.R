library(ggplot2)
library(dplyr)

# Simulate data for scenarios
set.seed(42) # For reproducibility
years <- rep(2000:2020, times = 4)
scenario <- rep(1:4, each = 21)
temp_increase <- 0.05 * (years - 2000) # 0.5 degree per decade, consistent across scenarios
mean_temp <- 6 + temp_increase + 0.25

scenario_temp <- mean_temp
data <- data.frame(years, scenario, temp = scenario_temp, mean_temp= mean_temp)

data$temp[which(data$scenario %in% c(1,2))] = 6

data$se <- c(0.2,0.5,0.2,0.5)[data$scenario]
data$lo <- data$temp - 2*data$se
data$hi <- data$temp + 2*data$se
data$shrink <- FALSE

# copy the data to include a 
data2 <- data
data2$shrink <- TRUE
data2$lo[which(data2$scenario==1)] <- data2$temp[which(data2$scenario==1)] - 2*seq(0.2,0.1, length.out=21)
data2$lo[which(data2$scenario==2)] <- data2$temp[which(data2$scenario==2)] - 2*seq(0.5,0.1, length.out=21)
data2$lo[which(data2$scenario==3)] <- data2$temp[which(data2$scenario==3)] - 2*seq(0.2,0.1, length.out=21)
data2$lo[which(data2$scenario==4)] <- data2$temp[which(data2$scenario==4)] - 2*seq(0.5,0.1, length.out=21)

data <- rbind(data, data2)
data$shrink <- as.factor(1 - as.numeric(data$shrink))
data$linestyle <- 1
data$linestyle[which(data$shrink==FALSE)] = 3

data$temp[which(data$shrink==1)] = data$temp[which(data$shrink==1)] + 0.5
data$lo[which(data$shrink==1)] = data$lo[which(data$shrink==1)] + 0.5
data$hi[which(data$shrink==1)] = data$hi[which(data$shrink==1)] + 0.5

# Plot without scenarios 3 and 6
ggplot(data) +
  geom_ribbon(aes(x = years, ymin = lo, ymax = hi, alpha = 0.3, group = shrink, fill = shrink)) +
  facet_wrap(~scenario, ncol = 2, labeller = as_labeller(c("1" = "Strong association, moves", 
                                                           "2" = "Weak association, moves", 
                                                           "3" = "Strong association, doesn't move", 
                                                           "4" = "Weak association, doesn't move"))) +
  labs(x = "Year", y = "Temperature (°C)") +
  geom_line(aes(x = years, y = mean_temp), color = "grey30", linewidth = 1, linetype = 2) +  # Mean temperature trend
  scale_fill_viridis_d(option = "magma", begin = 0.5, end = 0.8) + 
  scale_color_viridis_d(option = "magma", begin = 0.5, end = 0.8) + 
  theme_bw() +
  theme(legend.position = "none", axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_rect(fill = "white"))  # Modify facet label background
ggsave("conceptual_fig.png", width=7, height = 5)
# 
# # Simulate data for scenarios
# set.seed(42) # For reproducibility
# years <- rep(2000:2020, times = 4)
# scenario <- rep(1:4, each = 21)
# temp_increase <- 0.05 * (years - 2000) # 0.5 degree per decade, consistent across scenarios
# mean_temp <- 6 + temp_increase
# 
# scenario_temp <- mean_temp
# data <- data.frame(years, scenario, temp = scenario_temp, mean_temp= mean_temp)
# 
# data$temp[which(data$scenario %in% c(1,2))] = 6
# 
# data$se <- c(0.2,0.4,0.2,0.4)[data$scenario]
# data$id <- seq(1, nrow(data))
# 
# for(i in 1:nrow(data)) {
#   
#   dat <- data
#   q_df <- data.frame(q = seq(-1.96, 1.96, length.out=80))
#   q_df$qnorm = dat$se[i]*q_df$q
#   z <- seq(0.01,1, length.out=40)
#   q_df$dnorm <- c(z, rev(z))#dnorm(abs(q_df$q), 0, 1)
#   q_df$id <- data$id[i]
#   #dat$q <- q_df$q[i]
#   #dat$grp <- i
#   dat <- left_join(dat, q_df)
#   if(i == 1) {
#     data_all <- dat
#   } else {
#     data_all <- rbind(data_all, dat)
#   }
# }
# 
# data$shrink <- FALSE
# 
# # # copy the data to include a 
# # data2 <- data
# # data2$shrink <- TRUE
# # data2$lo[which(data2$scenario==1)] <- data2$temp[which(data2$scenario==1)] - 2*seq(0.2,0.1, length.out=21)
# # 
# # data2$lo[which(data2$scenario==2)] <- data2$temp[which(data2$scenario==2)] - 2*seq(0.4,0.1, length.out=21)
# # 
# # data2$lo[which(data2$scenario==3)] <- data2$temp[which(data2$scenario==3)] - 2*seq(0.2,0.1, length.out=21)
# # 
# # data2$lo[which(data2$scenario==4)] <- data2$temp[which(data2$scenario==4)] - 2*seq(0.4,0.1, length.out=21)
# # 
# # data <- rbind(data, data2)
# # data$shrink <- as.factor(1 - as.numeric(data$shrink))
# # data$linestyle <- 1
# # data$linestyle[which(data$shrink==FALSE)] = 3
# 
# 
# make_lines <- function(years = 2000:2020, temp_trend = rep(6, 21), 
#                        se = rep(0.2, 21), shrink = FALSE, n_lines = 80) {
#   alphas <- seq(0.01,1, length.out=n_lines/2)
#   for(i in 1:n_lines) {
#     d <- data.frame(years = years, se = se)
#     d$temp <- temp_trend + se*seq(-1.96, 1.96, length.out=n_lines)[i]
#     if(shrink == TRUE) {
#       if(i <= n_lines / 2) { 
#         # then lower se ramps down from the input value to half
#         ses <- se * seq(1, 0.5, length.out=length(se))
#         d$temp <- temp_trend + ses*seq(-1.96, 1.96, length.out=n_lines)[i]
#       }
#     }
#     
#     d$alpha <- c(alphas, rev(alphas))[i]
#     d$id <- i
#     if(i==1) {
#       df <- d
#     } else {
#       df <- rbind(df,d)
#     }
#   }
#   return(df)
# }
# 
# # create scenarios with no shrinking niches
# df1 <- make_lines()
# df1$scenario <- 1
# df2 <- make_lines(se = rep(0.4,21))
# df2$scenario <- 2
# df3 <- make_lines(temp_trend = seq(6,7,length.out=21))
# df3$scenario <- 3
# df4 <- make_lines(temp_trend = seq(6,7,length.out=21),se = rep(0.4,21))
# df4$scenario <- 4
# data_all <- rbind(df1,df2,df3,df4)
# data_all$grp <- as.factor(1)
# data_all$shrink <- FALSE
# 
# data_all <- dplyr::filter(data_all, id %in% c(1,80))
# 
# data_all$temp <- data_all$temp + 0.3
# 
# 
# 
# df_wider <- data_all %>%
#   tidyr::pivot_wider(
#     names_from = id,    
#     values_from = temp 
#   )
# names(df_wider)[7:8] = c("lo","hi")
# 
# df1 <- make_lines(shrink = TRUE)
# df1$scenario <- 1
# df2 <- make_lines(se = rep(0.4,21), shrink = TRUE)
# df2$scenario <- 2
# df3 <- make_lines(temp_trend = seq(6,7,length.out=21), shrink = TRUE)
# df3$scenario <- 3
# df4 <- make_lines(temp_trend = seq(6,7,length.out=21),se = rep(0.4,21), shrink = TRUE)
# df4$scenario <- 4
# data_shrink <- rbind(df1,df2,df3,df4)
# 
# data_shrink <- dplyr::filter(data_shrink, id %in% c(1,80))
# 
# df_wider2 <- data_shrink %>%
#   tidyr::pivot_wider(
#     names_from = id,    
#     values_from = temp 
#   )
# names(df_wider2)[7:8] = c("lo","hi")
# 
# data_shrink$grp <- as.factor(1)
# data_shrink$shrink <- TRUE
# 
# data_all <- rbind(data_all, data_shrink)
# data_all$shrink <- as.factor(1-as.numeric(data_all$shrink))
# 
# temptrend <- data.frame(years = 2000:2020, mean_temp = seq(6,7,length.out=21))
# data_all <- dplyr::left_join(data_all, temptrend)
# 
# ggplot(data_all) +
#   geom_line(aes(x = years, y = temp, group = id, alpha=alpha*0.02, col = shrink)) + 
#   #geom_ribbon(aes(x = years, ymin = lo, ymax = hi, alpha = 0.5)) +
#   facet_wrap(~scenario, ncol = 2, labeller = as_labeller(c("1" = "Strong association, moves", 
#                                                            "2" = "Weak association, moves", 
#                                                            "3" = "Strong association, doesn't move", 
#                                                            "4" = "Weak association, doesn't move"))) +
#   labs(x = "Year", y = "Temperature (°C)") +
#   geom_line(aes(x = years, y = mean_temp), color = "grey30", linewidth = 1, linetype = 2) +  # Mean temperature trend
#   #scale_fill_viridis_d(option = "magma", begin = 0.5, end = 0.8) + 
#   scale_color_viridis_d(option = "magma", begin = 0.5, end = 0.8) + 
#   theme_bw() +
#   theme(legend.position = "none", axis.ticks.x = element_blank(),
#         axis.text.x = element_blank(),
#         strip.background = element_rect(fill = "white", colour = "black"))  # Modify facet label background
# ggsave("conceptual_fig.png", width=7, height = 5)
# 
