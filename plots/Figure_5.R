library(ggplot2)


index = readRDS("output/temp_index_wc.rds")


g1 = ggplot(index, aes(year, est)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr),fill="red",alpha=0.3,col=NA) + 
  geom_line(alpha=0.7, col="red") + 
  xlab("Year") + 
  ylab("Estimated temperature index") + 
  theme_bw()

ggsave(plot = g1, filename="plots/Figure_5.png", width = 6, height = 5)
