library(ggplot2)
library(dplyr)
library(pals)

region = "goa"
all_temp = readRDS(paste0("output/temp_niche_",region,".rds"))
all_empirical = readRDS(paste0("output/empirical_temp_niche_",region,".rds"))

# are there species that are broadening their niche?
all_temp$enviro <- all_temp$enviro * temp_sd + temp_mean
summaries <- as.data.frame(all_temp) %>%
  dplyr::group_by(year, species) %>%
  dplyr::summarize(
    mean_est = mean(enviro),
    lo10 = quantile(enviro, 0.05),
    hi10 = quantile(enviro, 0.95),
    lo20 = quantile(enviro, 0.1),
    hi20 = quantile(enviro, 0.9),
    lo30 = quantile(enviro, 0.15),
    hi30 = quantile(enviro, 0.85),
    lo40 = quantile(enviro, 0.2),
    hi40 = quantile(enviro, 0.8),
    lo50 = quantile(enviro, 0.25),
    hi50 = quantile(enviro, 0.75)
  ) %>%
  as.data.frame()
summaries$mean_est[which(summaries$mean_est > 14)] <- NA
summaries$mean_est[which(summaries$mean_est < 2)] <- NA

# summarize empirical 90% CIs
all_empirical$enviro <- all_empirical$enviro * temp_sd + temp_mean
emp_summaries <- as.data.frame(all_empirical) %>%
  dplyr::group_by(year, species) %>%
  dplyr::summarize(
    empirical_lo10 = quantile(enviro, 0.05),
    empirical_hi10 = quantile(enviro, 0.95)
  ) %>%
  as.data.frame()

summaries <- dplyr::left_join(summaries, emp_summaries)

if(region=="goa") {
  summaries = dplyr::filter(summaries, species %in% c("Alaskan pink shrimp",
                                          "sidestripe shrimp",
                                          "eulachon",
                                          "shrimp unid.",
                                          "sablefish",
                                          "shortspine thornyhead",
                                          "magistrate armhook squid",
                                          "dusky and dark rockfishes unid.") == FALSE)
  
}

summaries$species = as.character(summaries$species)
summaries$species = paste0(toupper(substr(summaries$species,1,1)), 
                        substr(summaries$species,2,nchar(summaries$species)))


g1 <- ggplot(summaries, aes(year, mean_est)) +
  facet_wrap(~species, scale = "free") +
  geom_ribbon(aes(ymin = lo10, ymax = hi10), alpha = 0.7, fill = brewer.blues(6)[1]) +
  geom_ribbon(aes(ymin = lo20, ymax = hi20), alpha = 0.7, fill = brewer.blues(6)[2]) +
  geom_ribbon(aes(ymin = lo30, ymax = hi30), alpha = 0.7, fill = brewer.blues(6)[3]) +
  geom_ribbon(aes(ymin = lo40, ymax = hi40), alpha = 0.7, fill = brewer.blues(6)[4]) +
  geom_ribbon(aes(ymin = lo50, ymax = hi50), alpha = 0.7, fill = brewer.blues(6)[5]) +
  geom_line(col = brewer.blues(6)[6], alpha = 0.5) +
  theme_bw() +
  # geom_hline(aes(yintercept=enviro_min),col="grey30",linetype="dashed") +
  # geom_hline(aes(yintercept=enviro_hi),col="grey30",linetype="dashed") +
  ylab("Temperature (C)") +
  xlab("Year") +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 7))

ggsave(plot = g1, filename=paste0("plots/Figure_S4",region,".png"), width=7,height=7)

g1 = g1 + 
  geom_line(aes(year, empirical_lo10), col = "red", linetype = "dashed") +
  geom_line(aes(year, empirical_hi10), col = "red", linetype = "dashed")
ggsave(plot = g1, filename=paste0("plots/Figure_S4_alt",region,".png"), width=7,height=7)

# are there species that are broadening their niche?
g1 <- ggplot(summaries, aes(year, hi10 - lo10)) +
  geom_line() +
  facet_wrap(~species, scale = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 7)) + 
  xlab("Year") +
  ylab("95% - 5% CI")
ggsave(plot = g1, filename=paste0("plots/Figure_S6",region,".png"), width=7,height=7)


