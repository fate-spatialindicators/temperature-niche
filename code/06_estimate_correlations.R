library(tidyr)

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
cor_goa$species[which(cor_goa$species=="spiny dogfish")] = "north pacific spiny dogfish"
cor_bc$species = as.character(cor_bc$species)
#cor_bc$species[which(cor_bc$species=="pacific spiny dogfish")] = "north pacific spiny dogfish"
cor_wc$species = as.character(cor_wc$species)
cor_wc$species[which(cor_wc$species=="pacific spiny dogfish")] = "north pacific spiny dogfish"

df = rbind(cor_goa, cor_bc, cor_wc)

df = pivot_wider(df, names_from=region, values_from=rho)

df$species = paste0(toupper(substr(df$species,1,1)), 
                    substr(df$species,2,nchar(df$species)))