library(dplyr)
library(ggplot2)

d <- readRDS("output/all_correlations_combined.rds")

# transform from z-space to cor space
d$ci_hi <- (exp(2 * d$ci_hi) - 1) / (exp(2 * d$ci_hi) + 1)
d$ci_lo <- (exp(2 * d$ci_lo) - 1) / (exp(2 * d$ci_lo) + 1)

d$species = as.character(d$species)
d$species = paste0(toupper(substr(d$species,1,1)), 
                           substr(d$species,2,nchar(d$species)))
d$species[which(d$species == "North pacific spiny dogfish")] = "North Pacific spiny dogfish"

ggplot(d, aes(year, r)) + 
  geom_hline(aes(yintercept=0), alpha=0.5, col = "red") +
  geom_line() + 
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi)) + 
  facet_wrap(~species) + 
  theme_bw() + 
  theme(strip.background = element_rect(fill = "white")) +
  theme(strip.text.x = element_text(size = 6),
        axis.text = element_text(size = 7)) + xlab("Year") + 
  ylab("Correlation")
  
ggsave(filename="plots/Figure_S7b.png", width = 7, height = 7)



