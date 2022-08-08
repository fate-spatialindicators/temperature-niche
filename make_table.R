df <- readRDS("output/wc/models.RDS")
df_wc = data.frame(species = as.character(unique(df$species)),
                   region="West Coast")

df <- readRDS("output/goa/models.RDS")
df_goa = data.frame(species = as.character(unique(df$species)),
                   region="GOA")

df <- readRDS("output/bc/models.RDS")
df_bc = data.frame(species = as.character(unique(df$species)),
                    region="BC")

df = rbind(df_bc, df_goa, df_wc)

# long to wide
df_all = data.frame(species = unique(df$species), "GOA" = "", "WC"="", "BC"="")
df_all$GOA[which(df_all$species%in%df_goa$species)] = "X"
df_all$WC[which(df_all$species%in%df_wc$species)] = "X"
df_all$BC[which(df_all$species%in%df_bc$species)] = "X"
