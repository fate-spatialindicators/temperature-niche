library(ggplot2)

df_goa = read.csv(file=here(paste0("output/","goa","_output.csv")), 
                  stringsAsFactors = FALSE)
df_bc = read.csv(file=here(paste0("output/","bc","_output.csv")), 
                 stringsAsFactors = FALSE)
df_wc = read.csv(file=here(paste0("output/","wc","_output.csv")), 
                 stringsAsFactors = FALSE)
df_goa$region = "GOA"
df_bc$region = "BC"
df_wc$region = "COW"

# drop shrimp from GOA
df_goa = dplyr::filter(df_goa, species %in% c("Alaskan pink shrimp",
                                             "sidestripe shrimp",
                                             "eulachon",
                                             "shrimp unid.",
                                             "sablefish",
                                             "shortspine thornyhead",
                                             "magistrate armhook squid") == FALSE)

df = rbind(df_goa, df_bc, df_wc) # bind together
df$mid = 0.5*(df$hi + df$lo) # add midpoint

df_depth = dplyr::filter(df, depth_effect==TRUE) %>% 
  dplyr::select(species, lo, mid, hi, region) %>%
  dplyr::rename(depth_lo = lo, depth_mid = mid, depth_hi = hi)
df_nodepth = dplyr::filter(df, depth_effect==FALSE) %>% 
  dplyr::select(species, lo, mid, hi, region)
df = dplyr::left_join(df_depth,df_nodepth)

df = dplyr::rename(df, Region = region)

g1 = ggplot(df, aes(depth_mid, mid,col=Region)) + 
  geom_abline(aes(intercept=0,slope=1),col="red",alpha=0.6) + 
  geom_linerange(aes(xmin = depth_lo, xmax = depth_hi), alpha=0.3) +
  geom_linerange(aes(ymin = lo, ymax = hi), alpha = 0.3) +
  geom_point(alpha=0.3) + 
  xlab("Midpoint (depth included)") + 
  ylab("Midpoint (depth not included)") + 
  scale_color_viridis_d(end=0.8) + 
  theme_bw()

ggsave(plot = g1, filename="plots/Figure_S1.png", width = 6, height = 5)
