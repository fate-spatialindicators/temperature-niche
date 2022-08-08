library(dplyr)

d_2021 <- read.csv(file.choose())
d_2021 <- dplyr::select(d_2021, 
                        Year,
                        Latitude.Dd,
                        Longitude.Dd,
                        Common.Name,
                        Scientific.Name,
                        Cpue.Kgkm2,
                        Bottom.Temperature.C,
                        Depth.M,
                        Surface.Temperature.C,
                        Survey,
                        Species.Code,
                        Date.Time, 
                        Station) %>%
  dplyr::rename(year = Year,
                latitude_dd = Latitude.Dd,
                longitude_dd = Longitude.Dd,
                species = Common.Name,
                scientific_name = Scientific.Name,
                cpue_kg_km2 = Cpue.Kgkm2,
                temp = Bottom.Temperature.C,
                depth = Depth.M,
                surface_temperature = Surface.Temperature.C,
                survey = Survey,
                species_code = Species.Code,
                date = Date.Time)
d_2021$date <- unlist(lapply(strsplit(d_2021$date, " "), getElement, 1))
d_2021$date <- lubridate::parse_date_time(d_2021$date,orders = "mdy")
d_2021$day <- lubridate::day(d_2021$date)
d_2021$month <- lubridate::month(d_2021$date)
d_2021$species <- tolower(d_2021$species)
stations <- expand.grid(Station = unique(d_2021$Station), species = unique(d_2021$species),
                        stringsAsFactors = FALSE)
haul <- dplyr::group_by(d_2021, Station) %>%
  dplyr::summarise(year=year[1],
                   latitude_dd = latitude_dd[1],
                   longitude_dd = longitude_dd[1],
                   temp = temp[1],
                   depth = depth[1],
                   surface_temperature = surface_temperature[1],
                   survey = survey[1],
                   date = date[1],
                   day = day[1],
                   month = month[1])
stations <- dplyr::left_join(stations, haul)

stations <- dplyr::left_join(stations, 
                             dplyr::select(d_2021, Station, species, cpue_kg_km2))
spp <- dplyr::group_by(d_2021, species) %>%
  dplyr::summarise(species_code = species_code[1],
                   scientific_name = scientific_name[1] )
stations = dplyr::left_join(stations, spp)


d <- readRDS("survey_data/AK_BTS_all_spp.rds")
names(d) <- tolower(names(d))
# filter out goa
d <- dplyr::filter(d, survey == "GOA")

# rename to match nwfsc
d <- dplyr::rename(d,
  species = common_name,
  scientific_name = species_name,
  latitude_dd = latitude,
  longitude_dd = longitude,
  cpue_kg_km2 = cpue,
  temp = gear_temperature,
  depth = bottom_depth,
) 

stations <- dplyr::filter(stations, tolower(species) %in% tolower(unique(d$species))) %>%
  dplyr::select(-Station)

d <- rbind(d, stations)

# filter out 0 depths
d <- dplyr::filter(d, depth > 0)

# filter out years < 1990 (gear changes) and 2001 (limited sampling)
d <- dplyr::filter(d, year %in% c(1984, 1987, 2001) == FALSE)

saveRDS(d, "survey_data/joined_goa_data.rds")
