library(ggplot2)
library(dplyr)
library(pals)

temp_wc = readRDS(paste0("output/temp_niche_wc.rds"))
temp_bc = readRDS(paste0("output/temp_niche_bc.rds"))
temp_goa = readRDS(paste0("output/temp_niche_goa.rds"))
temp_wc$region <- "COW"
temp_bc$region <- "BC"
temp_goa$region <- "GOA"

summaries <- rbind(temp_wc, temp_bc, temp_goa)
# Name formatting
summaries$species = as.character(summaries$species)
summaries$species = paste0(toupper(substr(summaries$species,1,1)), 
                        substr(summaries$species,2,nchar(summaries$species)))

# # also add in the mean temp index
wc_index = readRDS("output/temp_index_wc.rds")
goa_index = readRDS("output/temp_index_goa.rds")
bc_index = readRDS("output/temp_index_bc.rds")
temp_indx = rbind(wc_index, goa_index, bc_index)
temp_index = dplyr::group_by(temp_indx, year) %>%
  dplyr::summarise(m = mean(est)) %>%
  dplyr::filter(!is.na(m), year>=2003, year %in% goa_index$year)

summaries$species[which(summaries$species == "North pacific spiny dogfish")] <- "Spiny dogfish"

summaries$region <- as.factor(summaries$region)
levels(summaries$region) <- c("GOA","BC","COW")
g1 <- ggplot(summaries, aes(year, mean_enviro, fill=region, col = region)) +
  #facet_wrap(~species, scale = "free") +
  facet_wrap(~species) +
  geom_ribbon(aes(ymin = lo10_enviro, ymax = hi10_enviro), alpha = 0.2, col=NA) +
  scale_color_viridis_d(option="magma",begin=0.2, end=0.7) + 
  scale_fill_viridis_d(option="magma",begin=0.2,end=0.7) + 
  geom_line(size=1) +
  theme_bw() +
  # geom_hline(aes(yintercept=enviro_min),col="grey30",linetype="dashed") +
  # geom_hline(aes(yintercept=enviro_hi),col="grey30",linetype="dashed") +
  ylab("Temperature (°C)") +
  xlab("Year") +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 7))

ggsave(plot = g1, filename=paste0("plots/Figure_4_by_region.png"), width=7,height=7)




all_p <- readRDS("output/all_proportions_combined.rds")
levels(all_p$region) <- c("GOA","BC","COW")
all_p$spp <- paste0(toupper(substr(all_p$spp,1,1)), substr(all_p$spp,2,nchar(all_p$spp)))

g1 <- ggplot(all_p, aes(year, p, col = region)) +
  #facet_wrap(~species, scale = "free") +
  facet_wrap(~spp) +
  scale_color_viridis_d(option="magma",begin=0.2, end=0.7) + 
  geom_line(size=1) +
  theme_bw() +
  # geom_hline(aes(yintercept=enviro_min),col="grey30",linetype="dashed") +
  # geom_hline(aes(yintercept=enviro_hi),col="grey30",linetype="dashed") +
  ylab("Temperature (°C)") +
  xlab("Year") +
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 7))

