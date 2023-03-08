library(ggplot2)
library(dplyr)
library(pals)

all_temp = readRDS(paste0("output/temp_niche_combined.rds"))

# are there species that are broadening their niche?
summaries <- as.data.frame(all_temp) %>%
  dplyr::group_by(year, species) %>%
  dplyr::summarize(
    mean_est = mean(mean_enviro),
    lo10 = quantile(lo10_enviro, 0.05),
    hi10 = quantile(hi10_enviro, 0.95),
    lo20 = quantile(lo20_enviro, 0.1),
    hi20 = quantile(hi20_enviro, 0.9),
    lo30 = quantile(lo30_enviro, 0.15),
    hi30 = quantile(hi30_enviro, 0.85),
    lo40 = quantile(lo40_enviro, 0.2),
    hi40 = quantile(hi40_enviro, 0.8),
    lo50 = quantile(lo50_enviro, 0.25),
    hi50 = quantile(hi50_enviro, 0.75)
  ) %>%
  as.data.frame()
summaries$mean_est[which(summaries$mean_est > 14)] <- NA
summaries$mean_est[which(summaries$mean_est < 2)] <- NA

# Name formatting
summaries$species = as.character(summaries$species)
summaries$species = paste0(toupper(substr(summaries$species,1,1)), 
                        substr(summaries$species,2,nchar(summaries$species)))

# also add in the mean temp index
wc_index = readRDS("output/temp_index_wc.rds")
goa_index = readRDS("output/temp_index_goa.rds")
bc_index = readRDS("output/temp_index_bc.rds")
temp_indx = rbind(wc_index, goa_index, bc_index)
temp_index = dplyr::group_by(temp_indx, year) %>%
  dplyr::summarise(m = mean(est)) %>%
  dplyr::filter(!is.na(m), year>=2003, year %in% goa_index$year)

g1 <- ggplot(dplyr::filter(summaries, year>=2003), aes(year, mean_est)) +
  facet_wrap(~species, scale = "free") +
  geom_ribbon(aes(ymin = lo10, ymax = hi10), alpha = 0.7, fill = brewer.blues(6)[1]) +
  geom_ribbon(aes(ymin = lo20, ymax = hi20), alpha = 0.7, fill = brewer.blues(6)[2]) +
  geom_ribbon(aes(ymin = lo30, ymax = hi30), alpha = 0.7, fill = brewer.blues(6)[3]) +
  geom_ribbon(aes(ymin = lo40, ymax = hi40), alpha = 0.7, fill = brewer.blues(6)[4]) +
  geom_ribbon(aes(ymin = lo50, ymax = hi50), alpha = 0.7, fill = brewer.blues(6)[5]) +
  geom_line(col = brewer.blues(6)[6], alpha = 0.5) +
  geom_line(data=temp_index, aes(year,m),col="red", alpha = 0.5) + 
  theme_bw() +
  # geom_hline(aes(yintercept=enviro_min),col="grey30",linetype="dashed") +
  # geom_hline(aes(yintercept=enviro_hi),col="grey30",linetype="dashed") +
  ylab("Temperature (C)") +
  xlab("Year") +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 7))

ggsave(plot = g1, filename=paste0("plots/Figure_4.png"), width=7,height=7)

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


