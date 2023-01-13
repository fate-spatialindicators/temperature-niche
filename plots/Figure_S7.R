library(ggplot2)
library(dplyr)
library(pals)

region = "goa"
all_temp = readRDS(paste0("output/temp_niche_",region,".rds"))

# are there species that are broadening their niche?
all_temp$enviro <- all_temp$enviro * temp_sd + temp_mean
goa_summaries <- as.data.frame(all_temp) %>%
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
goa_summaries$mean_est[which(goa_summaries$mean_est > 14)] <- NA
goa_summaries$mean_est[which(goa_summaries$mean_est < 2)] <- NA
goa_summaries$species = as.character(goa_summaries$species)
goa_summaries$species = paste0(toupper(substr(goa_summaries$species,1,1)), 
                           substr(goa_summaries$species,2,nchar(goa_summaries$species)))

region = "wc"
all_temp = readRDS(paste0("output/temp_niche_",region,".rds"))

# are there species that are broadening their niche?
all_temp$enviro <- all_temp$enviro * temp_sd + temp_mean
wc_summaries <- as.data.frame(all_temp) %>%
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
wc_summaries$mean_est[which(wc_summaries$mean_est > 14)] <- NA
wc_summaries$mean_est[which(wc_summaries$mean_est < 2)] <- NA

wc_summaries$region <- "WC"
goa_summaries$region <- "GOA"
goa_summaries$species <- tolower(goa_summaries$species)
summaries <- rbind(wc_summaries, goa_summaries)
summaries$species <- as.character(summaries$species)

summaries <- dplyr::group_by(summaries, species, year) %>%
  dplyr::mutate(n = length(unique(region))) %>%
  dplyr::filter(n == 2) %>%
  dplyr::select(-n)

g1 <- ggplot(summaries, aes(year, mean_est, fill = region, col = region)) +
  facet_wrap(~species, scale = "free") +
  geom_ribbon(aes(ymin = lo10, ymax = hi10), alpha = 0.3, col=NA) +
  #geom_ribbon(aes(ymin = lo20, ymax = hi20), alpha = 0.4) +
  #geom_ribbon(aes(ymin = lo30, ymax = hi30), alpha = 0.5) +
  #geom_ribbon(aes(ymin = lo40, ymax = hi40), alpha = 0.6) +
  geom_ribbon(aes(ymin = lo50, ymax = hi50), alpha = 0.7, col=NA) +
  geom_line(alpha = 0.5) +
  theme_bw() +
  # geom_hline(aes(yintercept=enviro_min),col="grey30",linetype="dashed") +
  # geom_hline(aes(yintercept=enviro_hi),col="grey30",linetype="dashed") +
  ylab("Temperature (C)") +
  xlab("Year") +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 7))

ggsave(plot = g1, filename=paste0("plots/Figure_S7",region,".png"), width=7,height=7)
