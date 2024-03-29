library(ggplot2)
library(dplyr)
library(pals)

region = "GOA"
#summaries = readRDS(paste0("output/temp_niche_",region,".rds"))

if(region=="GOA") summaries = readRDS(paste0("output/temp_niche_combined_goa.rds"))
summaries$species = as.character(summaries$species)
summaries$species = paste0(toupper(substr(summaries$species,1,1)), 
                        substr(summaries$species,2,nchar(summaries$species)))

spp_for_brms <- readRDS("~/Documents/Github projects/consonants-static/output/spp_for_brms.rds")
summaries <- dplyr::filter(summaries, species %in% spp_for_brms)
#temp_index = readRDS(paste0("output/temp_index_",region,".rds"))

g1 <- dplyr::filter(summaries, year %in% c(2003:2021)) %>%
  ggplot(aes(year, mean_enviro)) +
  facet_wrap(~species, scale = "free") +
  geom_ribbon(aes(ymin = lo10_enviro, ymax = hi10_enviro), alpha = 0.7, fill = brewer.blues(6)[1]) +
  geom_ribbon(aes(ymin = lo20_enviro, ymax = hi20_enviro), alpha = 0.7, fill = brewer.blues(6)[2]) +
  geom_ribbon(aes(ymin = lo30_enviro, ymax = hi30_enviro), alpha = 0.7, fill = brewer.blues(6)[3]) +
  geom_ribbon(aes(ymin = lo40_enviro, ymax = hi40_enviro), alpha = 0.7, fill = brewer.blues(6)[4]) +
  geom_ribbon(aes(ymin = lo50_enviro, ymax = hi50_enviro), alpha = 0.7, fill = brewer.blues(6)[5]) +
  geom_line(col = brewer.blues(6)[6], alpha = 0.5) +
  theme_bw() +
  #geom_line(data = temp_index, aes(year, est), col="red",alpha=0.5) + 
  # geom_hline(aes(yintercept=enviro_min),col="grey30",linetype="dashed") +
  # geom_hline(aes(yintercept=enviro_hi),col="grey30",linetype="dashed") +
  ylab(expression(paste("Temperature ", degree, "C"))) +
  xlab("Year") +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 7)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5,hjust=0.3))

ggsave(plot = g1, filename=paste0("plots/Figure_S4",region,".png"), width=8,height=8)



# are there species that are broadening their niche?
# g1 <- ggplot(summaries, aes(year, hi10_enviro - lo10_enviro)) +
#   #geom_point() + 
#   geom_line() +
#   facet_wrap(~species, scale = "free_y") +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "white")) +
#   theme(strip.text.x = element_text(size = 6),
#         axis.text = element_text(size = 7)) + 
#   xlab("Year") +
#   ylab("95% - 5% CI") + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5,hjust=0.3))
# ggsave(plot = g1, filename=paste0("plots/Figure_S6",region,".png"), width=8,height=8)




# combo <- dplyr::left_join(summaries, temp_index)
# combo$se_enviro <- as.numeric((combo$hi10_enviro - combo$lo10_enviro) / 3.29)
#   
# fit <- lm((mean_enviro - est) ~ year * species, weights = 1/(se_enviro^2), data = combo)
# summary(fit)
# 
# 
# fit <- lm((hi10_enviro - lo10_enviro) ~ year * species, weights = 1/(se_enviro^2), data = combo)
# summary(fit)
# 
# fit <- lm((hi10_enviro - lo10_enviro) ~ est * species, weights = 1/(se_enviro^2), data = combo)
# summary(fit)
# 
# library(glmmTMB)
# fit <- glmmTMB((mean_enviro - est) ~ species * year, data = combo)
# summary(fit)
#   
