library(ggplot2)

# bring in various outputs -- use only quadratic results
wc = read.csv("output/wc_output.csv", 
              stringsAsFactors = FALSE)
goa = read.csv("output/goa_output.csv", 
               stringsAsFactors = FALSE)
bc = read.csv("output/bc_output.csv", 
               stringsAsFactors = FALSE)

# goa = dplyr::filter(goa, species %in% c("Alaskan pink shrimp",
#                                               "sidestripe shrimp",
#                                               "eulachon",
#                                               "shrimp unid.",
#                                               "sablefish",
#                                               "shortspine thornyhead",
#                                               "magistrate armhook squid") == FALSE)

wc$Region = "COW"
goa$Region="GOA"
bc$Region="BC"
df = rbind(wc,goa,bc) %>% 
  dplyr::filter(quadratic==TRUE) 

# drop out uncertain quadratic models
df = dplyr::mutate(df, quad_cv = abs(b_env2_se/b_env2)) %>%
  dplyr::filter(quad_cv < 0.5)

df$mid = 0.5*(df$hi + df$lo)
df$mid_se = sqrt((0.5^2)*(df$hi_se^2) + (0.5^2)*(df$lo_se^2))

means = dplyr::group_by(df, Region) %>%
  dplyr::summarise(mid = mean(mid,na.rm=T),
                   range = mean(range,na.rm=T))
df$Region <- factor(df$Region, levels = c("GOA","BC","COW"))
means$Region <- factor(means$Region, levels = c("GOA","BC","COW"))
mycols <- viridis::magma(3, begin=0.2, end=0.8)

g1 = ggplot(dplyr::filter(df,mid>0,range>0), aes(mid, range, col=Region)) + 
  geom_vline(data = means, aes(xintercept=mid, col=Region)) +
  geom_hline(data = means, aes(yintercept=range, col=Region)) +
  geom_point(size=3,alpha=0.5) + 
  #scale_color_manual(values = mycols) + 
  xlab("Midpoint of estimated thermal niche") + 
  ylab("Range of thermal niche") + 
  theme_bw() +
  scale_fill_viridis_d(begin = 0.2, end=0.8, option="magma") + 
  scale_color_viridis_d(begin = 0.2, end=0.8, option="magma")


ggsave(plot = g1, filename="plots/Figure_S2.png", width = 6, height = 5)
