library(dplyr)
library(ggplot2)

## All widespead fish species in BC trawl surveys
spp <- tolower(c(
  # "Aleutian Skate",
  "Big Skate",
  "Longnose Skate",
  "Sandpaper Skate",
  "North Pacific Spiny Dogfish",
  # "Brown Cat Shark",
  "Spotted Ratfish",
  "Pacific Tomcod",
  "Walleye Pollock",
  "Pacific Cod",
  "Lingcod",
  "Pacific Hake",
  "Buffalo Sculpin",
  # "Cabezon",
  # "Pacifc Staghorn Sculpin",
  # "Red Irish Lord",
  # "Sturgeon Poacher",
  "Bigmouth Sculpin",
  "Kelp Greenling",
  "Threadfin Sculpin",
  "Bigfin Eelpout",
  "Black Eelpout",
  # "Wattled Eelpout",
  "Blackbelly Eelpout",
  # "Shiner Perch",
  # "Snake Prickleback",
  # "Wolf Eel"
  "Pacific Sand Lance",
  # "Pacific Herring", # missing from synopsis data
  "Sablefish",
  
## ROCKFISH
  "Aurora Rockfish",
  "Bocaccio",
  "Canary Rockfish",
  "Chilipepper",
  "Copper Rockfish", # small sample
  "Darkblotched Rockfish", # need predictions still
  "Dusky Rockfish",
  "Greenstriped Rockfish",
  "Harlequin Rockfish",
  "Pacific Ocean Perch",
  "Pygmy Rockfish",
  "Quillback Rockfish",
  "Redbanded Rockfish",
  "Redstripe Rockfish",
  "Rosethorn Rockfish",
  "Rougheye/Blackspotted Rockfish Complex",
  "Sharpchin Rockfish",
  # "Shortbelly Rockfish", # missing from synopsis data
  "Shortraker Rockfish",
  "Silvergray Rockfish",
  "Splitnose Rockfish",
  "Widow Rockfish", # schooling
  "Yellowmouth Rockfish",
  "Yellowtail Rockfish", # schooling
  "Yelloweye Rockfish",
  "Longspine Thornyhead",
  "Shortspine Thornyhead",
  
## FLATFISH
  "Pacific Halibut",
  "Arrowtooth Flounder",
  # "Butter Sole", # too localized
  # "C-O Sole", # too localized
  "Curlfin Sole",
  "Dover Sole",
  "English Sole",
  "Flathead Sole",
  "Pacific Sanddab",
  "Petrale Sole",
  "Rex Sole",
  "Southern Rock Sole",
  "Slender Sole"
  # "Sand Sole", # too localized
  # "Starry Flounder" # too localized
))

## trial a smaller number
# spp <- tolower(c(
#   "Dover sole", "arrowtooth flounder", "rex sole",
#   "English sole", "sablefish", "Pacific cod", "north pacific spiny dogfish",
#   "longnose skate", "big skate", "Pacific ocean perch", "pacific halibut"
# ))


# Trawl survey data -------------------------------------------------------

.file <- "survey_data/bc-synoptic-trawls.rds"
if (!file.exists(.file)) {
  dt <- list()
  for (i in seq_along(spp)) {
    .spp <- gsub(" ", "-", gsub("\\/", "-", tolower(spp[i])))
    # dt[[i]] <- gfdata::get_survey_sets(.spp, ssid = c(1, 3, 4, 16),
    #   join_sample_ids = TRUE)
    # or {
    dt[[i]] <- readRDS(paste0("../gfsynopsis/report/data-cache/", .spp, ".rds"))
    dt[[i]] <- dt[[i]]$survey_sets
    # }
  }
  dt <- dplyr::bind_rows(dt)
  dt <- dt %>% rename(survey_series_id = survey_series_id.x)
  dt2 <- dplyr::filter(dt, survey_series_id %in% c(1, 3, 4, 16)) %>%
    dplyr::mutate(cpue_kg_km2 = density_kgpm2*1000000,
      fishing_event_id = as.integer(fishing_event_id),
      year = as.integer(year)
      ) %>%
    dplyr::select(year,
      # ssid = survey_series_id,
      survey = survey_abbrev,
      species = species_common_name,
      scientific_name = species_science_name,
      # longitude_dd = longitude, #going to rely on fishing event id for merge with environmental data
      # latitude_dd = latitude,
      depth = depth_m,
      cpue_kg_km2,
      fishing_event_id
    ) %>%
    distinct()
  saveRDS(dt2, file = .file)
} else {
  dt <- readRDS(.file)
}


# Environmental data ------------------------------------------------------

# New data:
# denv <- gfdata::get_sensor_data_trawl(ssid = c(1, 3, 4, 16), spread_attributes = FALSE)
# saveRDS(denv, "survey_data/bc-synoptic-env-raw.rds")
denv <- readRDS("survey_data/bc-synoptic-env-raw.rds")

# Old data:
# denv_old <- gfdata::get_table("FE_SALINITY_DO")
# saveRDS(denv_old, "survey_data/bc-do-salinity-table.rds")
denv_old <- readRDS("survey_data/bc-do-salinity-table.rds")
names(denv_old) <- tolower(names(denv_old))
denv_old_do <- denv_old %>%
  group_by(fishing_event_id) %>%
  summarise(
    avg = mean(do, na.rm = TRUE),
    min = min(do, na.rm = TRUE),
    max = max(do, na.rm = TRUE),
    start_time = min(fe_event_time),
    end_time = max(fe_event_time),
    year = lubridate::year(fe_event_time),
    .groups = "drop_last"
  ) %>%
  mutate(attribute = "do_mlpL") %>%
  distinct()

denv_old_salinity <- denv_old %>%
  group_by(fishing_event_id) %>%
  summarise(
    avg = mean(salinity, na.rm = TRUE),
    min = min(salinity, na.rm = TRUE),
    max = max(salinity, na.rm = TRUE),
    start_time = min(fe_event_time),
    end_time = max(fe_event_time),
    year = lubridate::year(fe_event_time),
    .groups = "drop_last"
  ) %>%
  mutate(attribute = "salinity_PSU") %>%
  distinct()

denv2 <- select(
  denv, fishing_event_id, attribute, avg,
  min, max, start_time, end_time, year
)

denv2 <- bind_rows(denv2, denv_old_do) %>%
  bind_rows(denv_old_salinity)

do <- denv2 %>%
  filter(attribute == "do_mlpL") %>%
  rename(do = avg) %>%
  select(year, fishing_event_id, do)

temp <- denv2 %>%
  filter(attribute == "temperature_C") %>%
  rename(temperature = avg) %>%
  select(year, fishing_event_id, temperature)

sal <- denv2 %>%
  filter(attribute == "salinity_PSU") %>%
  rename(salinity = avg) %>%
  select(year, fishing_event_id, salinity)

d <- left_join(dt, do) %>%
  left_join(temp) %>%
  left_join(sal)

group_by(d, year) %>%
  summarise(frac_na = round(sum(is.na(do)) / n(), 2)) %>%
  knitr::kable()
group_by(d, year) %>%
  summarise(frac_na = round(sum(is.na(temperature)) / n(), 2)) %>%
  knitr::kable()
group_by(d, year) %>%
  summarise(frac_na = round(sum(is.na(salinity)) / n(), 2)) %>%
  knitr::kable()

ggplot(d, aes(longitude, latitude, colour = salinity)) +
  geom_point() +
  facet_wrap(~year)

ggplot(d, aes(year, salinity, group = year)) +
  geom_boxplot()

ggplot(d, aes(longitude, latitude, colour = do)) +
  geom_point() +
  facet_wrap(~year)

# ignore 2007, bad DO data, nobody can figure out why:
d <- mutate(d, do = if_else(year != 2007, do, NA_real_))

ggplot(d, aes(longitude, latitude, colour = temperature)) +
  geom_point() +
  facet_wrap(~year)

env_data <- select(
  d, fishing_event_id, do, temperature, salinity,
  depth_m, longitude, latitude
) %>%
  mutate(fishing_event_id = as.integer(fishing_event_id)) %>%
  distinct()
saveRDS(env_data, "survey_data/bc-synoptic-env.rds")


# read and join with
env_data <- readRDS("survey_data/bc-synoptic-env.rds")
trawl_data <- readRDS("survey_data/bc-synoptic-trawls.rds")
dat <- left_join(trawl_data, env_data, by = "fishing_event_id") %>% rename(temp = temperature, 
  # o2 = do, # much fewer years of complete data if o2 added in
  longitude_dd = longitude, latitude_dd = latitude) %>% select(-depth_m, -salinity)

# filter by species that occur in 10% of hauls
threshold = 0.1

keep = dat %>% 
  mutate(occur = ifelse(cpue_kg_km2 > 0,1,0)) %>%
  group_by(year, species) %>% 
  summarize(p = sum(occur)/n()) %>% 
  group_by(species) %>% 
  summarize(mean_p = mean(p, na.rm=T)) %>% 
  filter(mean_p >= threshold)

dat2 <- dplyr::filter(dat, species %in% keep$species)

sort(unique(dat$species)) # list all species
sort(unique(dat2$species)) # list species that occur in >10% of hauls

# remove trawls that are missing tempurature data
dat2 <- dat2[complete.cases(dat2), ]

saveRDS(dat2, "survey_data/joined_bc_data.rds")
