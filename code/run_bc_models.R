# devtools::install_github("pbs-assess/sdmTMB")
library(sdmTMB)
library(dplyr)
library(sp)

dat = readRDS("survey_data/joined_bc_data.rds")

# UTM transformation
dat_ll = dat
coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
proj4string(dat_ll) <- CRS("+proj=longlat +datum=WGS84")
# convert to utm with spTransform
dat_utm = spTransform(dat_ll, 
  CRS("+proj=utm +zone=9 +datum=WGS84 +units=km"))
# convert back from sp object to data frame
dat = as.data.frame(dat_utm)
dat = dplyr::rename(dat, longitude = longitude_dd,
latitude = latitude_dd)

# filter out species with < 50 samples in a year
summary = read.csv("output/summary_statistics_bc.csv")
summary = dplyr::filter(summary, min_n >= 50) # 50 used for other areas, might try 30 for BC
dat = dplyr::filter(dat, species %in% summary$species)

# create grid of spp. Run each 2x, with and without
# quadratic roots. without quadratic, use a smooth function instead
df = expand.grid("species" = unique(dat$species),
  spatial_only=c(FALSE), 
  depth_effect = c(TRUE),
  time_varying = c(FALSE),
  quadratic = c(TRUE,FALSE),
  covariate = c("temp")
)
saveRDS(df, "output/bc/models.RDS")


for(i in 1:nrow(df)) {
  
  sub = dplyr::filter(dat, species == df$species[i])
  
  # rescale variables
  sub$depth = as.numeric(scale(log(sub$depth)))
  # mean depth: 5.614747, sd = 0.896874
  # mean temp: 6.800925, sd temp = 1.983694
  sub$temp = as.numeric((scale(sub$temp)))
  
  # not including o2 right now because many fewer years data
  # will need to make new bc dataframe
  # sub$o2 = as.numeric(scale(log(sub$o2)))
  
  # drop points with missing depth or temp values
  sub = dplyr::filter(sub, !is.na(depth), !is.na(temp))
  
  # rename variables to make code generic
  sub = dplyr::rename(sub, enviro = as.character(df$covariate[i]))
  
  # make spde
  spde <- try(make_mesh(sub, c("longitude","latitude"),
    cutoff=17.5), silent=TRUE)
  if(class(spde) != "try-error") {
    formula = paste0("cpue_kg_km2 ~ -1")
    
    time_formula = "~ -1"
      
    if(df$quadratic[i]==TRUE) {
      formula = paste0(formula, " + ", 
        "enviro", " + I(","enviro","^2)")
      time_varying = NULL
      time = "year"
    } else {
      formula = paste0(formula, " + ", 
               "s(enviro,k=3)")
      time_varying = NULL
      time = "year"        
    }
      
    formula = paste0(formula, " + as.factor(year)")
    
    if(df$depth_effect[i]==TRUE) {
      formula = paste0(formula, " + depth + I(depth^2)")
    }
    
    # fit model
    m <- try(sdmTMB(
      formula = as.formula(formula),
      time_varying = time_varying,
      spde = spde,
      time = time,
      family = tweedie(link = "log"),
      data = sub,
      spatial_only = df$spatial_only[i],
      quadratic_roots = df$quadratic[i]
    ), silent=TRUE)
    
    if(class(m)!="try-error") {
        saveRDS(m, file=paste0("output/bc/model_",i,".rds"))
    }
  } # end try on spde
  
}
