library(ggplot2)
library(here)
library(dplyr)
library(sdmTMB)

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
  dplyr::filter(depth_effect == TRUE)
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

# for each region, create a set of limits for each species of the temps the species have been observed at
cow <- readRDS("survey_data/joined_nwfsc_data.rds")
cow_limits <- dplyr::filter(cow, cpue_kg_km2 > 0) %>%
                     dplyr::group_by(species) %>% 
                     dplyr::summarise(lo = min(temp,na.rm=T), hi = max(temp,na.rm=T))
bc <- readRDS("survey_data/bc_data_2021.rds")
bc_limits <- dplyr::filter(bc, cpue_kg_km2 > 0) %>%
  dplyr::group_by(species) %>% 
  dplyr::summarise(lo = min(temperature,na.rm=T), hi = max(temperature,na.rm=T))
goa <- readRDS("survey_data/joined_goa_data.rds")
goa_limits <- dplyr::filter(goa, cpue_kg_km2 > 0) %>%
  dplyr::group_by(species) %>% 
  dplyr::summarise(lo = min(temp,na.rm=T), hi = max(temp,na.rm=T))

# add in region-specific limits, and filter data to exclude points beond ranges that haven't been observed
cow_limits$species <- paste0(toupper(substr(cow_limits$species,1,1)), substr(cow_limits$species,2,length(cow_limits$species)))
df_cow <- dplyr::filter(expanded_df, region == "COW") %>%
  dplyr::left_join(cow_limits) %>%
  dplyr::filter(temp > lo, temp < hi)
bc_limits$species <- paste0(toupper(substr(bc_limits$species,1,1)), substr(bc_limits$species,2,length(bc_limits$species)))
df_bc <- dplyr::filter(expanded_df, region == "BC") %>%
  dplyr::left_join(bc_limits) %>%
  dplyr::filter(temp > lo, temp < hi)
goa_limits$species <- paste0(toupper(substr(goa_limits$species,1,1)), substr(goa_limits$species,2,length(goa_limits$species)))
df_goa <- dplyr::filter(expanded_df, region == "GOA") %>%
  dplyr::left_join(goa_limits) %>%
  dplyr::filter(temp > lo, temp < hi)

expanded_df_orig <- expanded_df

expanded_df <- rbind(df_cow, df_bc, df_goa)

# calculate predicted effect
expanded_df$effect <- expanded_df$temp * expanded_df$b_env + (expanded_df$temp^2) * expanded_df$b_env2

expanded_df = dplyr::rename(expanded_df, Region=region)

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



species_table <- read.csv("species_table.csv")
species_table <- dplyr::filter(species_table, n_region != 1)
cow_limits$species[which(cow_limits=="pacific spiny dogfish")] = "north pacific spiny dogfish"
goa_limits$species[which(goa_limits$species=="spiny dogfish")] = "north pacific spiny dogfish"
all_limits <- rbind(bc_limits, cow_limits, goa_limits)
range_all <- dplyr::filter(all_limits, tolower(species) %in% species_table$species) %>%
  dplyr::group_by(species) %>%
  dplyr::summarize(lo = min(lo), hi = max(hi))

range_all$species <- paste0(toupper(substr(range_all$species,1,1)), substr(range_all$species,2,length(range_all$species)))

expanded_df2 <- dplyr::left_join(expanded_df_orig, range_all)
expanded_df2 <- dplyr::filter(expanded_df2,
                             temp > lo, temp < hi)

species_table$b0 <- NA
species_table$b1 <- NA
species_table$b2 <- NA
for(i in 1:nrow(species_table)) {
  this_species <- species_table$species[i]
  f = readRDS(file = paste0("output/all/", this_species, ".rds"))
  aics = rep(NA, 4)
  for(j in 1:length(aics)) {
    s = try(sanity(f[[j]]), silent=TRUE)
    if(class(s) != "try-error") {
      if(s$hessian_ok * s$eigen_values_ok * s$nlminb_ok == 1) {
        # keep
        aics[j] = AIC(f[[j]])
      }
    }
  }
  best <- which.min
  # grab coefs from model 4
  coefs = try(as.numeric(unlist(tidy(f[[4]])[1:2,"estimate"])), silent=TRUE)
  if(class(coefs) != "try-error") {
    species_table$b1[i] = coefs[1]
    species_table$b2[i] = coefs[2]
    
    # calculate mean intercept
    indx <- which(tidy(f[[4]])$term %in% c("regionBC","regionCOW","regionGOA"))
    species_table$b0[i] <- mean(as.numeric(unlist(tidy(f[[4]])[indx,"estimate"])))
  }
}

species_table$species <- paste0(toupper(substr(species_table$species,1,1)), substr(species_table$species,2,length(species_table$species)))
expanded_df2 <- dplyr::left_join(expanded_df2, species_table[,c("species","b1","b2")])
expanded_df2$effect <- expanded_df2$b1 * expanded_df2$temp + expanded_df2$b2 * expanded_df2$temp * expanded_df2$temp
expanded_df2 <- dplyr::arrange(expanded_df2, species, temp)

expanded_df2 <- dplyr::rename(expanded_df2, Region = region)
expanded_df2$Region = "Combined"


combo <- rbind(expanded_df, dplyr::select(expanded_df2, -b1, -b2))

combo$Region <- factor(combo$Region, levels = c("BC","COW","GOA"))

d <- dplyr::filter(combo, Region!="Combined")
d$Region <- factor(d$Region, levels = c("GOA","BC","COW"))

g2 <- d %>%
  ggplot(aes(temp, effect,col=Region)) + 
  #geom_line(aes(size=1), alpha=0.5) + 
  geom_line(size=1,alpha=0.8) + 
  scale_color_viridis_d(end=0.8) + 
  facet_wrap(~species,ncol=7, scale="free_y") +
  theme_bw() + xlab("Bottom temperature") + 
  ylab("Estimated effect (link space)") + 
  theme(strip.background =element_rect(fill="white")) + 
  geom_line(data = dplyr::filter(combo, Region=="Combined"), col="grey10", size=1.4, alpha=0.8)

ggsave(g2, file = "plots/Figure_2_alt_combined.png", height=5, width=12)



g3 <- d %>%
  dplyr::filter(species %in% c("Arrowtooth flounder", "Longnose skate", "Pacific cod", "Petrale sole")) %>%
  ggplot(aes(temp, effect,col=Region)) + 
  #geom_line(aes(size=1), alpha=0.5) + 
  geom_line(size=1,alpha=0.8) + 
  scale_color_viridis_d(end=0.8) + 
  facet_wrap(~species,ncol=2, scale="free_y") +
  theme_bw() + xlab("Bottom temperature") + 
  ylab("Estimated effect (link space)") + 
  theme(strip.background =element_rect(fill="white")) + 
  geom_line(data = dplyr::filter(combo, Region=="Combined",species %in% c("Arrowtooth flounder", "Longnose skate", "Pacific cod", "Petrale sole")), col="grey10", size=1.4, alpha=0.8) +
  theme(text = element_text(size=20))
ggsave(g3, file = "plots/Figure_2_alt_combined_foreccwo.png")

# calculate quadratic lo / hi
species_table$lo = NA
species_table$hi = NA
for(i in 1:nrow(species_table)) {
  a = species_table$b2[i]
  b = species_table$b1[i]
  c = species_table$b0[i]
  roots = (-b -sqrt(b*b-4*a*c))/(2*a)
  roots[2] = (-b +sqrt(b*b-4*a*c))/(2*a)
  species_table$lo[i] = max(min(roots), 0)
  species_table$hi[i] = max(roots)
}

species_table$diff <- species_table$hi - species_table$lo
species_table$mid <- (species_table$hi - species_table$lo)/2
ranges <- dplyr::filter(species_table, !is.na(diff)) %>%
  dplyr::arrange(diff)
ranges$species = factor(ranges$species, levels = ranges$species)

basesize <- 8
g4 <- ggplot(ranges, aes(species, mid)) + 
  #geom_vline(aes(xintercept=0)) + 
  geom_linerange(aes(ymin=lo,ymax=hi), col=viridis(1), size=1.3) + 
  ylab("Temperature (°C)") + 
  xlab("") + 
  coord_flip() + 
  theme_bw(base_size=basesize) + 
  theme(text = element_text(size=15))
ggsave(g4, file = "plots/Ranges_fig_foreccwo.png")

d = data.frame(x = seq(-3,3,by=0.1))
a = 0.8
b = 0
c = -0.1
d$y = a + b*d$x + c*d$x*d$x

text_size=2
parabola = ggplot(d, aes(x+4,y)) + 
  geom_line(col="darkblue",size=1.3,alpha=0.3) + 
  ylab("Estimated effect") + 
  xlab("Temperature (°C)") +
  geom_point(data = data.frame(x=c(-2.83,0,2.83),y=c(0,0,0)), col="darkblue",size=2) + 
  #geom_text(data = data.frame(x=-3,y=0.03), label="(a)", size=text_size) + 
  #geom_text(data = data.frame(x=2.83-0.17,y=0.03), label="(c)", size=text_size) +
  #geom_text(data = data.frame(x=-0.17,y=0.03), label="(b)", size=text_size) + 
  #geom_text(data = data.frame(x=-0.17,y=0.73), label="(e)") + 
  #geom_text(data = data.frame(x=-0.17,y=0.83), label="(d)") + 
  geom_line(data = data.frame(x = c(-2.83,2.83), y = c(0,0)), col="darkblue",linetype="dashed") +
  #geom_line(data = data.frame(x = c(0,1), y = c(0.7,0.7)), col="darkblue",linetype="dashed") +
  geom_line(data = data.frame(x = c(0,0), y = c(0,0.8)), col="darkblue",linetype="dashed") +
  theme_bw() + 
  theme(text = element_text(size=8))
library(patchwork)

g5 <- g4 + theme(text = element_text(size=8))
parabola + g5 + plot_layout(widths = c(1,1), heights = c(1,1)) +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")")
ggsave("plots/Combined_parabola_ranges.png")
