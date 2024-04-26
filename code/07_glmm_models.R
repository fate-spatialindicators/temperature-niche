library(dplyr)
library(pals)
library(sp)
library(glmmTMB)
library(brms)

set.seed(1234)

dat <- readRDS("output/temp_niche_combined.rds")

dat <- dplyr::filter(dat, year >= 2003)

dat$width <- dat$hi10_enviro - dat$lo10_enviro


wc_index = readRDS("output/temp_index_wc.rds")
goa_index = readRDS("output/temp_index_goa.rds")
bc_index = readRDS("output/temp_index_bc.rds")
temp_indx = rbind(wc_index, goa_index, bc_index)
temp_index = dplyr::group_by(temp_indx, year) %>%
  dplyr::summarise(mean_temp = mean(est)) %>%
  dplyr::filter(!is.na(mean_temp), year>=2003, year %in% goa_index$year)

temp_index$diff <- c(NA, diff(temp_index$mean_temp))


dat <- dplyr::left_join(dat, temp_index)

library(stringr)
rho = dplyr::group_by(dat, species) %>% 
  dplyr::summarise(rho = cor(mean_enviro, avg_temp)) %>%
  arrange(rho)
rho$species <- str_to_title(rho$species)
rho$species[which(rho$species == "North Pacific Spiny Dogfish")] = "Spiny Dogfish"
rho$species <- factor(rho$species, levels = rho$species)


# ggplot(rho, aes(species, rho, col=rho)) + 
#   geom_point(size=4, alpha=1) + 
#   xlab("") + ylab("Correlation (Mean thermal niche, temperature)") + 
#   theme_bw() + 
#   scale_colour_gradient2() + 
#   coord_flip() + theme(text = element_text(size=8)) +
#   theme(legend.position = "none")
# ggsave("plots/Correlations.png")

#fit <- glmmTMB(diff_width ~ mean_temp + (1|species), data = dat)
# fit <- glmmTMB(width ~ mean_temp * (species), data = dat)
# 
# # test for trend through time
# coefs <- data.frame(species = unique(dat$species), coef = 0, p=0)
# for(i in 1:nrow(coefs)) {
#   fit <- lm(mean_enviro ~ year, data = dplyr::filter(dat, species == coefs$species[i]))
#   coefs$coef[i] <- summary(fit)$coefficients[2,1]
#   coefs$p[i] <- summary(fit)$coefficients[2,4]
# }

dat <- dplyr::group_by(dat, species) %>% 
  dplyr::mutate(diff_width = c(NA, diff(width)),
                diff_temp = c(NA, diff(avg_temp)))

sub <- dplyr::filter(dat, !is.na(diff_width), !is.na(diff_temp))
sub$avg_temp2 <- sub$avg_temp^2
sub$diff_temp2 <- sub$diff_temp^2



fit <- brm(diff_width ~ -1 + diff_temp + (-1+diff_temp|species), data = sub,
           chains = 4,
           iter = 4000)

# fit2 <- brm(width ~ 1 + mean_enviro + (1 + mean_enviro|species), data = sub,
#            chains = 4,
#            iter = 3000)

coefs <- as.data.frame(coef(fit)$species[,,1])
names(coefs) <- c("estimate", "se","lo95","hi95")
coefs$Species <- rownames(coefs)

# calculate correlation between predicted and obs by species
pred <- predict(fit)
sub$pred <- pred[,"Estimate"]
rho <- dplyr::group_by(sub, species) %>%
  dplyr::summarise(rho = cor(pred, diff_width)) %>%
  dplyr::rename(Species = species, Corr=rho) %>% as.data.frame()
coefs <- dplyr::left_join(coefs, rho)

coefs$Species <- paste0(toupper(substr(coefs$Species,1,1)), substr(coefs$Species,2,nchar(coefs$Species)))
coefs <- dplyr::arrange(coefs, estimate)

coefs$Species[which(coefs$Species=="North pacific spiny dogfish")] = "Pacific Spiny Dogfish"

coefs$Speciesf <- factor(coefs$Species, levels = coefs$Species)  

p1 <- dplyr::filter(coefs, Speciesf %in% c("Pacific grenadier","Giant grenadier") == FALSE) %>%
ggplot(aes(Speciesf, estimate, col=Corr)) + 
  geom_hline(aes(yintercept=0), col="red", alpha=0.5) + 
  geom_linerange(aes(ymin=lo95, ymax=hi95), col="grey70") +
  geom_linerange(aes(ymin=lo95, ymax=hi95)) + 
  geom_point() + 
  coord_flip() + 
  theme_bw() + 
  ylab(expression(paste(Delta, "Niche width (",degree,"C)"))) + xlab("") + 
  scale_color_gradient2() + 
  labs(color = "Correlation")

ggsave(p1, file="plots/Figure_5_brms.png", height = 6, width = 6)


# fit <- brm(width ~ (1+avg_temp|species), data = sub,
#            chains = 4,
#            iter = 2000)


# ggplot(dat, aes(year, mean_enviro)) + 
#   geom_linerange(aes(ymin=lo10_enviro, ymax = hi10_enviro), col=viridis(1), 
#                  alpha=0.3,position=position_dodge2(preserve = "single", width = 1)) + 
#   geom_smooth() + 
#   theme_bw() + ylab("Temperature") + xlab("Year") + theme(text = element_text(size=16))
# ggsave("plots/Trend_temp.png")



