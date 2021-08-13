devtools::install_github("pbs-assess/sdmTMB", "priors-experimental")
library(sdmTMB)
library(dplyr)
library(sp)

dat = readRDS("survey_data/joined_goa_data.rds")

# UTM transformation
dat_ll = dat
coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
proj4string(dat_ll) <- CRS("+proj=longlat +datum=WGS84")
# convert to utm with spTransform
dat_utm = spTransform(dat_ll, 
  CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
# convert back from sp object to data frame
dat = as.data.frame(dat_utm)
# convert to km
dat = dplyr::rename(dat, longitude = longitude_dd, 
  latitude = latitude_dd)
dat$latitude = dat$latitude /1000
dat$longitude = dat$longitude /1000

# filter out species with < 50 samples in a year
summary = read.csv("output/summary_statistics_goa.csv")
summary = dplyr::filter(summary, min_n >= 50)
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
saveRDS(df, "output/goa/models.RDS")

# note that not all years are included: 1990-1999 (3 year survey), 2003-2019

for(i in 1:nrow(df)) {
  
  # rescale variables
  sub = dplyr::filter(dat, species == df$species[i])
  sub$depth = as.numeric(scale(log(sub$depth)))
  sub$temp = as.numeric((scale(sub$temp)))
  
  # drop out earlier years
  #sub = dplyr::filter(sub, as.numeric(year) >= 2003)
  
  # drop points with missing values
  sub = dplyr::filter(sub, 
    !is.na(temp),!is.na(depth))
  
  # rename variables to make code generic
  sub = dplyr::rename(sub, enviro = as.character(df$covariate[i]))
  
  # make spde
  spde <- try(make_mesh(sub, c("longitude","latitude"),
                        cutoff=25), silent=TRUE)
  if(class(spde) != "try-error") {
    formula = paste0("cpue_kg_km2 ~ -1")
    
    time_formula = "~ -1"
    if(df$time_varying[i]==TRUE) {
      time_formula = paste0(time_formula, " + ", 
                            "enviro", " + I(","enviro","^2)")
      time_varying = as.formula(time_formula)
      time = "year"
    } else {
      
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
      
    }
    formula = paste0(formula, " + as.factor(year)")
    
    if(df$depth_effect[i]==TRUE) {
      formula = paste0(formula, " + depth + I(depth^2)")
    }  
  
    # use PC prior for matern model
    priors = sdmTMBpriors(
      matern_s = pc_matern(
        range_gt = 5, range_prob = 0.05,
        sigma_lt = 25, sigma_prob = 0.05
      )
    )    
  # fit model
    m <- try(sdmTMB(
      formula = as.formula(formula),
      time_varying = time_varying,
      spde = spde,
      time = time,
      family = tweedie(link = "log"),
      data = sub,
      priors=priors,
      spatial_only = df$spatial_only[i],
      control = sdmTMBcontrol(quadratic_roots = df$quadratic[i]),
    ), silent=TRUE)
    
  #sd_report <- summary(m$sd_report)
  #params <- as.data.frame(sd_report[grep("quadratic", 
  #  row.names(sd_report)), ])
  
  if(class(m)!="try-error") {
    #if(depth_filter==FALSE) {
      saveRDS(m, file=paste0("output/goa/model_",i,".rds"))
    #} else {
    #  saveRDS(m, file=paste0("output/goa/model_depthFilter_",i,".rds"))
    #}
  }
  } # end try on spde
  
}
