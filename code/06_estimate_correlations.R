library(tidyr)
library(ggplot2)
library(ggcorrplot)

temp_index = readRDS("output/temp_index_wc.rds")
df = readRDS(paste0("output/temp_niche_","wc",".rds"))
# are there species that are broadening their niche?
summaries <- as.data.frame(df) %>%
  dplyr::group_by(year, species) %>%
  dplyr::summarize(
    mean_est = mean(mean_enviro)
  ) %>%
  as.data.frame()

summaries = dplyr::left_join(summaries, temp_index)

cor_wc = dplyr::group_by(summaries, species) %>% 
  dplyr::summarise(rho = cor(est, mean_est)) %>% 
  dplyr::arrange(-abs(rho)) %>%
  as.data.frame()

temp_index = readRDS("output/temp_index_bc.rds")
df = readRDS(paste0("output/temp_niche_","bc",".rds"))
# are there species that are broadening their niche?
summaries <- as.data.frame(df) %>%
  dplyr::group_by(year, species) %>%
  dplyr::summarize(
    mean_est = mean(mean_enviro)
  ) %>%
  as.data.frame()

summaries = dplyr::left_join(summaries, temp_index)

cor_bc = dplyr::group_by(summaries, species) %>% 
  dplyr::summarise(rho = cor(est, mean_est)) %>% 
  dplyr::arrange(-abs(rho)) %>%
  as.data.frame()

temp_index = readRDS("output/temp_index_goa.rds")
df = readRDS(paste0("output/temp_niche_","goa",".rds"))
# are there species that are broadening their niche?
summaries <- as.data.frame(df) %>%
  dplyr::group_by(year, species) %>%
  dplyr::summarize(
    mean_est = mean(mean_enviro)
  ) %>%
  as.data.frame()

summaries = dplyr::left_join(summaries, temp_index)

cor_goa = dplyr::group_by(summaries, species) %>% 
  dplyr::summarise(rho = cor(est, mean_est)) %>% 
  dplyr::arrange(-abs(rho)) %>%
  as.data.frame()

cor_goa$region = "GOA"
cor_wc$region = "COW"
cor_bc$region = "BC"
cor_goa$species = tolower(cor_goa$species)
cor_goa$species[which(cor_goa$species=="spiny dogfish")] = "Pacific Spiny Dogfish"
cor_bc$species = as.character(cor_bc$species)
#cor_bc$species[which(cor_bc$species=="pacific spiny dogfish")] = "north pacific spiny dogfish"
cor_wc$species = as.character(cor_wc$species)
cor_wc$species[which(cor_wc$species=="pacific spiny dogfish")] = "Pacific Spiny Dogfish"

df = rbind(cor_goa, cor_bc, cor_wc)

df = pivot_wider(df, names_from=region, values_from=rho)

df$species = paste0(toupper(substr(df$species,1,1)), 
                    substr(df$species,2,nchar(df$species)))

df$species[which(df$species=="North pacific spiny dogfish")] = "Pacific Spiny Dogfish"

df <- dplyr::arrange(df, species)


goa <- df[,c("species","GOA")]
names(goa)[2] <- "Corr"
bc <- df[,c("species","BC")]
names(bc)[2] <- "Corr"
cow <- df[,c("species","COW")]
names(cow)[2] <- "Corr"
goa$region <- "GOA"
bc$region <- "BC"
cow$region <- "COW"
df <- rbind(goa, bc, cow)
  
df$species <- factor(df$species, levels = rev(unique(df$species)))
df$region <- factor(df$region, levels = c("GOA","BC","COW"))

# filter the 30 species that are occuring in 2_regions
spp_for_brms <- readRDS("~/Documents/Github projects/consonants-static/output/spp_for_brms.rds")

spp_for_brms[which(spp_for_brms == "North pacific spiny dogfish")] = "Pacific Spiny Dogfish"

dplyr::filter(df, !is.na(Corr), species %in% spp_for_brms) %>%
ggplot(aes(region, species, col = Corr)) + 
  geom_point(size=4) + 
  scale_color_gradient2() + 
  ylab("") + xlab("") + 
  theme_bw() + 
  theme(axis.text.y = element_text(size=10))

ggsave("plots/derived_correlations.png", height=6, width=5)  

