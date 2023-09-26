df <- readRDS("output/wc/models.RDS")
df_wc = data.frame(species = as.character(unique(df$species)),
                   region="West Coast")
cow <- readRDS("survey_data/joined_nwfsc_data.rds")
cow <- dplyr::group_by(cow, species) %>%
  dplyr::summarise(scientific_name = scientific_name[1])
df_wc <- dplyr::left_join(df_wc, cow)

df <- readRDS("output/goa/models.RDS")
df_goa = data.frame(species = as.character(unique(df$species)),
                   region="GOA")
goa <- readRDS("survey_data/joined_goa_data.rds")
goa <- dplyr::group_by(goa, species) %>%
  dplyr::summarise(scientific_name = scientific_name[1])
df_goa <- dplyr::left_join(df_goa, goa)

df <- readRDS("output/bc/models.RDS")
df_bc = data.frame(species = as.character(unique(df$species)),
                    region="BC")
bc <- readRDS("survey_data/bc_data_2021.rds")
bc <- dplyr::group_by(bc, species) %>%
  dplyr::summarise(scientific_name = scientific_name[1])
df_bc <- dplyr::left_join(df_bc, bc)

df = rbind(df_bc, df_goa, df_wc)

# long to wide
df_all = data.frame(species = unique(df$species), "GOA" = "", "WC"="", "BC"="")
df_all$GOA[which(df_all$species%in%df_goa$species)] = "X"
df_all$WC[which(df_all$species%in%df_wc$species)] = "X"
df_all$BC[which(df_all$species%in%df_bc$species)] = "X"

df_all <- dplyr::left_join(df_all, df) %>%
  dplyr::select(-region)

tab <- dplyr::group_by(df_all, species) %>%
  dplyr::summarise(GOA = GOA[1], WC = WC[1], BC = BC[1], 
                   scientific_name = scientific_name[1])
tab <- tab[,c("species", "scientific_name", "GOA","BC","WC")]
for(i in 1:nrow(tab)) {
  tab$species[i] <- paste(toupper(substr(tab$species[i],1,1)), substr(tab$species[i],2,nchar(tab$species[i])), sep="")
  tab$scientific_name[i] <- paste(toupper(substr(tab$scientific_name[i],1,1)), substr(tab$scientific_name[i],2,nchar(tab$scientific_name[i])), sep="")
}
colnames(tab) <- c("Species","Scientific name","GOA","BC","COW")
write.csv(tab,"output/Table_1.csv", row.names = FALSE)

