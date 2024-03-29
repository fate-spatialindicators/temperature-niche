library(ggplot2)

region="goa"
df = read.csv(file=here(paste0("output/",region,"_output.csv")), 
              stringsAsFactors = FALSE)

# sort by the midpoint, which is same as sorting by % above / below
df$mid = 0.5*(df$hi + df$lo)
df$mid_se = sqrt((0.5^2)*(df$hi_se^2) + (0.5^2)*(df$lo_se^2))
# drop the NAs and calculate the SE / mean for the quadratic effect - want to weed
# out the models with uncertain coefs
df_goa = dplyr::filter(df, !is.na(lo), !is.na(hi), covariate=="temp", depth_effect == TRUE) %>% 
  dplyr::mutate(quad_cv = b_env2_se/b_env2) %>% 
  dplyr::filter(abs(quad_cv) <= 1)
df_goa$species = tolower(df_goa$species)

region="bc"
df = read.csv(file=here(paste0("output/",region,"_output.csv")), 
              stringsAsFactors = FALSE)

# sort by the midpoint, which is same as sorting by % above / below
df$mid = 0.5*(df$hi + df$lo)
df$mid_se = sqrt((0.5^2)*(df$hi_se^2) + (0.5^2)*(df$lo_se^2))
# drop the NAs and calculate the SE / mean for the quadratic effect - want to weed
# out the models with uncertain coefs
df_bc = dplyr::filter(df, !is.na(lo), !is.na(hi), covariate=="temp", depth_effect == TRUE) %>% 
  dplyr::mutate(quad_cv = b_env2_se/b_env2) %>% 
  dplyr::filter(abs(quad_cv) <= 1)
df_bc$species = tolower(df_bc$species)

region="wc"
df = read.csv(file=here(paste0("output/",region,"_output.csv")), 
              stringsAsFactors = FALSE)

# sort by the midpoint, which is same as sorting by % above / below
df$mid = 0.5*(df$hi + df$lo)
df$mid_se = sqrt((0.5^2)*(df$hi_se^2) + (0.5^2)*(df$lo_se^2))
# drop the NAs and calculate the SE / mean for the quadratic effect - want to weed
# out the models with uncertain coefs
df_wc = dplyr::filter(df, !is.na(lo), !is.na(hi), covariate=="temp", depth_effect == TRUE) %>% 
  dplyr::mutate(quad_cv = b_env2_se/b_env2) %>% 
  dplyr::filter(abs(quad_cv) <= 1)

df_wc$region = "COW"
df_bc$region = "BC"
df_goa$region = "GOA"

# create grid of species x regions
all_spp = unique(c(df_wc$species,df_goa$species,df_bc$species))
grid = expand.grid(species = all_spp, region = c("COW","BC","GOA")) # 43 sp x 3 regions
df_all = left_join(grid, rbind(df_wc,df_bc,df_goa))

df_all$species = paste0(toupper(substr(df_all$species,1,1)), 
                    substr(df$species,2,nchar(df_all$species)))
df_all = dplyr::rename(df_all, Region = region)

png("plots/Figure_2.png")
g1 = df_all %>% 
  ggplot(aes(species, mid, col = Region)) + 
  #geom_hline(aes(yintercept=0), col="red",alpha=0.3) +
  geom_linerange(aes(ymin=lo, ymax = hi),alpha=0.5) + 
  geom_point(alpha=0.5) + 
  coord_flip() + 
  xlab("Species") + 
  ylab("") + 
  theme_bw() + 
  scale_color_viridis_d(end=0.8) + 
  theme(strip.background =element_rect(fill="white"))
g1
dev.off()
