library(ggplot2)
library(here)
library(dplyr)

df_goa <- read.csv(file=here(paste0("output/","goa","_output.csv")), 
              stringsAsFactors = FALSE)
df_bc <- read.csv(file=here(paste0("output/","bc","_output.csv")), 
                  stringsAsFactors = FALSE)
df_wc <- read.csv(file=here(paste0("output/","wc","_output.csv")), 
                 stringsAsFactors = FALSE)
df_wc$region <- "COW"
df_bc$region <- "BC"
df_goa$region <- "GOA"

df <- rbind(df_wc, df_bc, df_goa) %>%
  dplyr::filter(depth_effect == TRUE, converged == 1)
df$species <- tolower(df$species)

df$species = paste0(toupper(substr(df$species,1,1)), 
                    substr(df$species,2,nchar(df$species)))

# sort by the midpoint, which is same as sorting by % above / below
df$mid <- 0.5*(df$hi + df$lo)
df$mid_se <- sqrt((0.5^2)*(df$hi_se^2) + (0.5^2)*(df$lo_se^2))

# filter species with 2+ regions
df <- dplyr::group_by(df, species) %>% 
  dplyr::mutate(n = n()) %>% 
  dplyr::filter(n > 1) %>% 
  dplyr::select(-n)

# expand a dataframe of temp x species
df$ID <- seq(1,nrow(df))
expanded_df <- expand.grid(ID = unique(df$ID),
                          temp = seq(1,14,length.out=100))
expanded_df <- dplyr::left_join(expanded_df, df[,c("ID","species","b_env","b_env2","region")])
expanded_df <- dplyr::rename(expanded_df, Region = region)

# for each region, create a set of limits for each species of the temps the species have been observed at
cow <- readRDS("survey_data/joined_nwfsc_data.rds")
cow <- dplyr::filter(cow, cpue_kg_km2 > 0) %>%
                     dplyr::group_by(species) %>% 
                     dplyr::summarise(lo = min(temp,na.rm=T), hi = max(temp,na.rm=T))
bc <- readRDS("survey_data/bc_data_2021.rds")
bc <- dplyr::filter(bc, cpue_kg_km2 > 0) %>%
  dplyr::group_by(species) %>% 
  dplyr::summarise(lo = min(temperature,na.rm=T), hi = max(temperature,na.rm=T))
goa <- readRDS("survey_data/joined_goa_data.rds")
goa <- dplyr::filter(goa, cpue_kg_km2 > 0) %>%
  dplyr::group_by(species) %>% 
  dplyr::summarise(lo = min(temp,na.rm=T), hi = max(temp,na.rm=T))

# add in region-specific limits, and filter data to exclude points beond ranges that haven't been observed
cow$species <- paste0(toupper(substr(cow$species,1,1)), substr(cow$species,2,length(cow$species)))
df_cow <- dplyr::filter(expanded_df, Region == "COW") %>%
  dplyr::left_join(cow) %>%
  dplyr::filter(temp > lo, temp < hi)
bc$species <- paste0(toupper(substr(bc$species,1,1)), substr(bc$species,2,length(bc$species)))
df_bc <- dplyr::filter(expanded_df, Region == "BC") %>%
  dplyr::left_join(bc) %>%
  dplyr::filter(temp > lo, temp < hi)
goa$species <- paste0(toupper(substr(goa$species,1,1)), substr(goa$species,2,length(goa$species)))
df_goa <- dplyr::filter(expanded_df, Region == "GOA") %>%
  dplyr::left_join(goa) %>%
  dplyr::filter(temp > lo, temp < hi)

expanded_df <- rbind(df_cow, df_bc, df_goa)

# calculate predicted effect
expanded_df$effect <- expanded_df$temp * expanded_df$b_env + (expanded_df$temp^2) * expanded_df$b_env2

#expanded_df = dplyr::rename(expanded_df, Region=region)
g1 <- expanded_df %>%
  ggplot(aes(temp, effect,col=Region)) + 
  #geom_line(aes(size=1), alpha=0.5) + 
  geom_line() + 
  scale_color_viridis_d(end=0.8) + 
  facet_wrap(~species,ncol=3, scale="free_y") +
  theme_bw() + xlab("Bottom temperature") + 
  ylab("Estimated effect (link space)") + 
  theme(strip.background =element_rect(fill="white"))

ggsave(g1, file = "plots/Figure_2_alt.png", height=7, width=7)




