library(tidyverse)
library(ggplot2)
library(ggsidekick)
df <- read.csv("output/wc_output.csv")
# saveRDS(df,file=paste0("output/",region,"_output.rds"))

wc_df <- dplyr::filter(df, covariate == "temp", depth_effect == "FALSE") %>%
  dplyr::select(species, range, range_se)
wc_df$region <- "USA West Coast"

summaries <- read.csv("output/summary_statistics_wc.csv")
# filter out cases where spp haven'tbeen observed at least
# 20 times / year
summaries <- dplyr::filter(summaries, min_n >= 20)
wc_df <- dplyr::filter(wc_df, species %in% summaries$species)

# filter out the odd sharks and inverts
wc_df <- dplyr::filter(wc_df, species %in% c(
  "pacific spiny dogfish",
  "dungeness crab", "brown cat shark", "grooved tanner crab",
  "filetail cat shark"
) == FALSE)

df <- read.csv("output/goa_output.csv")

summaries <- read.csv("output/summary_statistics_wc.csv")
# filter out cases where spp haven'tbeen observed at least
# 20 times / year
summaries <- dplyr::filter(summaries, min_n >= 20)
df <- dplyr::filter(df, species %in% summaries$species)

goa_df <- dplyr::filter(df, covariate == "temp", depth_effect == "FALSE") %>%
  dplyr::select(species, range, range_se)
goa_df$region <- "Gulf of Alaska"

goa_df <- dplyr::filter(goa_df, species %in% c(
  "octopus unid.",
  "Alaskan pink shrimp", "sidestripe shrimp", "squid unid.",
  "shrimp unid.", "Pacific sleeper shark", "spiny dogfish",
  "", "magistrate armhook squid", "giant octopus",
  "chinook salmon", "chum salmon", "capelin", "eulachon",
  "sand lance unid.", "Pacific herring"
) == FALSE)

df <- rbind(wc_df, goa_df)
df$species <- as.character(df$species)
df$species <- tolower(df$species)

pdf("plots/Figure_2b_est_ranges.pdf", height = 8, width = 6)

ggplot(dplyr::arrange(df, region, species) %>% dplyr::filter(!is.na(range)), aes(species, range)) +
  geom_point(col = "dark blue", alpha = 0.6) +
  geom_errorbar(aes(ymin = range - range_se, ymax = range + range_se), col = "dark blue", alpha = 0.6) +
  coord_flip() +
  facet_wrap(~region) +
  ylim(0, 10) +
  ylab("Estimated temperature range") +
  xlab("Species")

dev.off()

# get species that occur in both surveys
overlap <- df$species[which(df$region == "USA West Coast")][match(df$species[which(df$region == "Gulf of Alaska")], df$species[which(df$region == "USA West Coast")])]

pdf("plots/Figure_2a_est_ranges.pdf", height = 5, width = 6)

ggplot(dplyr::arrange(df, region, species) %>% dplyr::filter(!is.na(range), species %in% overlap[!is.na(overlap)]), aes(species, range)) +
  geom_point(col = "dark blue", alpha = 0.6) +
  geom_errorbar(aes(ymin = range - range_se, ymax = range + range_se), col = "dark blue", alpha = 0.6) +
  coord_flip() +
  facet_wrap(~region) +
  ylim(0, 10) +
  ylab("Estimated temperature range") +
  xlab("Species")

dev.off()

write.csv(df, "output/combined_species.csv")
