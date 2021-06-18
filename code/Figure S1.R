library(tidyverse)
library(ggplot2)
library(ggsidekick)

df = read.csv("output/wc_output.csv")

summaries = read.csv("output/summary_statistics_wc.csv") 
# filter out cases where spp haven'tbeen observed at least
# 20 times / year
summaries = dplyr::filter(summaries, min_n >= 20)
df = dplyr::filter(df, species %in% summaries$species)

#saveRDS(df,file=paste0("output/",region,"_output.rds"))

wc_df = dplyr::filter(df, covariate=="temp") %>%
  dplyr::select(species,range,depth_effect) %>% 
  pivot_wider(names_from = depth_effect, values_from=range)
wc_df$region = "USA West Coast"

# filter out the odd sharks and inverts
wc_df = dplyr::filter(wc_df, species %in% c("pacific spiny dogfish",
  "dungeness crab","brown cat shark","grooved tanner crab",
  "filetail cat shark") == FALSE)


df = read.csv("output/goa_output.csv")
summaries = read.csv("output/summary_statistics_goa.csv") 
# filter out cases where spp haven'tbeen observed at least
# 20 times / year
summaries = dplyr::filter(summaries, min_n >= 20)
df = dplyr::filter(df, species %in% summaries$species)

goa_df = dplyr::filter(df, covariate=="temp") %>%
  dplyr::select(species,range,depth_effect) %>% 
  pivot_wider(names_from = depth_effect, values_from=range)
goa_df$region = "Gulf of Alaska"

goa_df = dplyr::filter(goa_df, species %in% c("octopus unid.",
  "Alaskan pink shrimp","sidestripe shrimp","squid unid.",
  "shrimp unid.","Pacific sleeper shark","spiny dogfish",
  "","magistrate armhook squid","giant octopus",
  "chinook salmon","chum salmon") == FALSE)

df = rbind(wc_df, goa_df) %>% 
  dplyr::rename("Depth included"="TRUE",
    "Depth omitted"="FALSE",
    "Species"="species") %>% 
  dplyr::mutate(`Depth included` = log(`Depth included`),
    `Depth omitted` = log(`Depth omitted`))

pdf("plots/Figure_S1_range_sensitivity.pdf",height=5,width=8)
ggplot(df, aes(`Depth included`,`Depth omitted`)) + 
  geom_point(col="dark blue",size=2,alpha=0.6) + 
  facet_wrap(~region) + theme_sleek()# + geom_text(aes(label=Species),size=3)
dev.off()

