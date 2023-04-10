library(dplyr)
library(pals)
library(sp)

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
  dplyr::summarise(rho = cor(mean_temp, mean_enviro)) %>%
  arrange(rho)
rho$species <- str_to_title(rho$species)
rho$species[which(rho$species == "North Pacific Spiny Dogfish")] = "Spiny Dogfish"
rho$species <- factor(rho$species, levels = rho$species)


ggplot(rho, aes(species, rho, col=rho)) + 
  geom_point(size=4, alpha=1) + 
  xlab("") + ylab("Correlation (Mean thermal niche, temperature)") + 
  theme_bw() + 
  scale_colour_gradient2() + 
  coord_flip() + theme(text = element_text(size=16)) +
  theme(legend.position = "none")
ggsave("plots/Correlations.png")

library(glmmTMB)
fit <- glmmTMB(diff_width ~ mean_temp + (1|species), data = dat)
fit <- glmmTMB(width ~ mean_temp * (species), data = dat)


dat <- dplyr::group_by(dat, species) %>% 
  dplyr::mutate(diff_width = c(NA, diff(width)))

fit <- glmmTMB(diff_width ~ diff + (1|species), data = dat)
fit <- glmmTMB(diff_width ~ species + diff : (species), data = dat)




fit <- glmmTMB(width ~ mean_temp, data = dat)
fit <- glmmTMB(diff_width ~ diff, data = dat)
fit <- glmmTMB(width ~ year, data = dat)


ggplot(dat, aes(year, mean_enviro)) + 
  geom_linerange(aes(ymin=lo10_enviro, ymax = hi10_enviro), col=viridis(1), 
                 alpha=0.3,position=position_dodge2(preserve = "single", width = 1)) + 
  geom_smooth() + 
  theme_bw() + ylab("Temperature") + xlab("Year") + theme(text = element_text(size=16))
ggsave("plots/Trend_temp.png")

library(mgcv)
fit <- gam(width ~ s(diff), data = dat)


