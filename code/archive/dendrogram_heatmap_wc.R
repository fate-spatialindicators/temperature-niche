# look at high level correlations between mean lats/depths and estimated affinities
library(ggdendro)
library(ggplot2)
library(dplyr)

region <- c("goa", "wc")[2]
df <- read.csv(paste0("output/", region, "_output.csv"), stringsAsFactors = FALSE) %>%
  dplyr::filter(depth_effect == TRUE) %>%
  dplyr::select(-X, -depth_effect)

# pull in summary data
summary <- read.csv(paste0("output/summary_statistics_", region, ".csv"),
  stringsAsFactors = FALSE
) %>%
  dplyr::select(-X)

species <- df$species[which(df$covariate == "temp")]
dftemp <- df[which(df$covariate == "temp"), ] %>%
  dplyr::select(-species, -covariate)
names(dftemp) <- paste0(names(dftemp), "_temp")
dfo2 <- df[which(df$covariate == "o2"), ] %>%
  dplyr::select(-species, -covariate)
names(dfo2) <- paste0(names(dfo2), "_o2")
df <- cbind(species, dftemp, dfo2)

# add in summary stats
df <- dplyr::left_join(df, summary)

# filter out variables of interest
df <- dplyr::select(
  df,
  species, low_temp, hi_temp, range_temp, reduction_temp,
  low_o2, hi_o2, range_o2, reduction_o2, weighted_depth,
  weighted_lat, weighted_lon
)

# omit species that didn't converge -- or converged for a single variable
df <- df[complete.cases(df), ] # 76 -> 46
df <- df[-c(33, 51), ]

scaled_df <- df %>%
  dplyr::select(-hi_temp, -hi_o2, -low_temp, -low_o2)
for (i in 2:ncol(scaled_df)) {
  scaled_df[, i] <- scale(df[, i])
}

# Run clustering
clust <- as.matrix(scaled_df[, -1])
rownames(clust) <- scaled_df$species
dendro <- as.dendrogram(hclust(d = dist(x = clust)))
g <- ggdendrogram(data = dendro, rotate = TRUE)

pdf("plots/dendro_wc.pdf")
g
dev.off()

order <- order.dendrogram(dendro)

long_df <- reshape2::melt(scaled_df, id = c("species"))

# Order the levels according to their position in the cluster
long_df$species <- factor(
  x = long_df$species,
  levels = scaled_df$species[order],
  ordered = TRUE
)


heatmap <- ggplot(data = long_df, aes(
  x = variable,
  y = species
)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

pdf("plots/heatmap_wc.pdf")
heatmap
dev.off()
