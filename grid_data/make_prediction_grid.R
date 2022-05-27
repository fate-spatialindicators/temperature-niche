library(raster)
library(sp)

# read in the grid cell data from the survey design
grid_cells = readxl::read_excel("grid_data/wc/Selection Set 2018 with Cell Corners.xlsx")
coordinates(grid_cells) <- c("Cent.Long", "Cent.Lat")
proj4string(grid_cells) <- CRS("+proj=longlat +datum=WGS84")
grid_cells <- spTransform(grid_cells, CRS("+proj=utm +zone=10 ellps=WGS84"))

# make prediction raster from grid_cell centroids (some cells are irregularly sized, would be good to use corners later)
#predict_raster = rasterFromXYZ(as.data.frame(cbind(X = grid_cells$Cent.Long, Y = grid_cells$Cent.Lat, depth = -999999))) # not working, grid likely irregular?

# make prediction raster roughly from grid_cell centroids, given standard cell dimensions (here in meters, converted from nm)
predict_raster = raster(grid_cells, resolution = c(2778,3704), vals = NULL)

## load custom bathymetry raster
bathy_hiRes <- raster("data/bathy_clipped")
bathy_hiRes = bathy_hiRes / 10 # units were originally decimeters, so convert to meters
#projectRaster(bathy_hiRes, crs = ("+proj=utm +zone=10 ellps=WGS84")) # slow because of high res, could maybe aggregate first or do all at once below?

# aggregate and project bathymetry to survey grid cells
bathy = projectRaster(bathy_hiRes, predict_raster, crs = ("+proj=utm +zone=10 ellps=WGS84"), method="bilinear")

# create matrix of point data with coordinates and depth from raster
wc_grid = as.data.frame(rasterToPoints(bathy))
colnames(wc_grid) = c("X", "Y", "depth")

# scale covariates
wc_grid$log_depth_scaled = scale(log(wc_grid$depth * -1))
wc_grid$log_depth_scaled2 = wc_grid$log_depth_scaled ^ 2
wc_grid$X = wc_grid$X/10000
wc_grid$Y = wc_grid$Y/10000

saveRDS(wc_grid, file=paste0("data/wc_grid.rds"))
