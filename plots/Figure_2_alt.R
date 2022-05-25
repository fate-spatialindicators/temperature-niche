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

# calculate predicted effect
expanded_df$effect <- expanded_df$temp * expanded_df$b_env + (expanded_df$temp^2) * expanded_df$b_env2

expanded_df = dplyr::rename(expanded_df, Region=region)
g1 <- expanded_df %>%
  ggplot(aes(temp, effect,col=Region)) + 
  geom_line() + 
  scale_color_viridis_d(end=0.8) + 
  facet_wrap(~species,ncol=3, scale="free_y") +
  theme_bw() + xlab("Bottom temperature") + 
  ylab("Estimated effect (link space)") + 
  theme(strip.background =element_rect(fill="white"))

ggsave(g1, file = "plots/Figure_2_alt.png", height=7, width=7)
