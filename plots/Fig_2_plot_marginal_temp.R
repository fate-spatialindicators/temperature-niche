library(dplyr)
library(pals)
library(sp)

set.seed(1234)
# species table
species_table <- read.csv("species_table.csv")
species_table = dplyr::mutate(species_table, 
                              GOA = ifelse(GOA=="x",1,0),
                              BC = ifelse(BC=="x",1,0),
                              WC = ifelse(WC=="x",1,0),
                              n_regions = GOA + BC + WC) %>%
  dplyr::filter(n_regions > 1)
spp <- readRDS("output/spp_for_brms.rds")
# filter out the 30 spp used in paper
spp <- spp[-which(spp == "Pacific ocean perch")]
spp <- c(spp, "Longspine thornyhead")
species_table <- dplyr::filter(species_table, species %in% tolower(spp))

mu_logdepth <- 5.21517
sd_logdepth <- 0.8465829

for (i in 1:nrow(species_table)) {
  
  this_species = species_table$species[i]
  
  # WC
  spp <- df_wc$id[which(df_wc$species==this_species)]
  pred_df_wc <- NULL
  
  # get the best fit model
  aic_table <- read.csv("combined_table.csv", header=TRUE)
  best_model <- which.min(aic_table[i,9:13])
  
  fit <- readRDS(file = paste0("output/all/", this_species, "_model",(best_model-1),".rds"))
  
  newdata = expand.grid(region = unique(fit$data$region),
             year = 2003,
             enviro = seq(2,14,length.out=100),
             X = fit$data$X[1],
             Y = fit$data$Y[1],
             depth = fit$data$depth[1],
             logdepth = fit$data$logdepth[1],
             logdepth2 = fit$data$logdepth2[1])
 newdata$enviro2 <- newdata$enviro^2
 newdata$scaled_enviro <- (newdata$enviro - mean(fit$data$enviro)) / sd(newdata$enviro)
 newdata$scaled_enviro2 <- newdata$scaled_enviro * newdata$scaled_enviro
 pred <- predict(fit, newdata)
 pred$species <- this_species
 if(i==1) {
   pred_all <- pred
 } else {
   pred_all <- rbind(pred, pred_all)
 }
 #ggplot(pred, aes(enviro, est_non_rf, col=region)) + geom_line()
 
}

saveRDS(pred_all, "output/estimated_temperature_marginals.rds")

pred_all$names <- sapply(pred_all$species, function(name) {
  paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
})
pred_all$Region <- pred_all$region
levels(pred_all$Region) <- c("GOA","BC","COW")

ggplot(pred_all, aes(enviro, est_non_rf, col=Region)) + 
  geom_line() + 
  scale_color_viridis_d(option="magma",begin=0.2, end=0.7) + 
  scale_fill_viridis_d(option="magma",begin=0.2, end=0.7) + 
  facet_wrap(~names, scale="free_y") + 
  xlab("Temperature (°C)") + 
  ylab("Marginal effect") + 
  theme_bw() + 
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 7))
ggsave("plots/Figure_2_marginal_temp.png", height = 7, width = 9)





