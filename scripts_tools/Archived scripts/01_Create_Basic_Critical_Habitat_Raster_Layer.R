####################################################
#### CREATE BASIC CRITICAL HABITAT RASTER LAYER ####
####################################################

# This script can be run once the polygon files have been exported from Google Earth Engine
# NOTE: needs > 18 GB of memory to run
# These scripts can be found:
# Potential: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FPotential_Critical_Habitat
# Likely: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FLikely_Critical_Habitat

# Install packages (if required)
list.of.packages <- c("sf", "fasterize", "raster", "arcgisbinding", "rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(sf)
library(fasterize)
library(raster)
library(rgeos)
library(terra)
library(arcgisbinding)
arc.check_product()

# set path variables (these will be the only lines that need changing in this script)
# path to where the GEE output shapefiles are stored
shapefile_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

# path to where you want the output saved
output_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/"

# read in shapefiles
likely <- read_sf(paste0(shapefile_path, "Likely_Critical_Habitat_Polygon.shp"))
potential <- read_sf(paste0(shapefile_path, "Potential_Critical_Habitat_Polygon.shp"))

# run the union to combine polygons (this may take a little while, don't worry it's a lot faster after this step)
likely_union <- st_as_sf(gUnaryUnion(as_Spatial(likely)))
potential_union <- st_as_sf(gUnaryUnion(as_Spatial(potential)))

# load example raster with appropriate extent (global) and resolution (1 km) properties (e.g. NatMod raster from the portal)
nat_mod <- arc.open('O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif')
raster <- as.raster(arc.raster(nat_mod))
raster

# create an empty raster with these properties
r <- raster(ext = extent(raster), resolution = res(raster))

# convert polygons to raster
likely_raster <- fasterize(likely_union, r, background = 0)
potential_raster <- fasterize(potential_union, r, background = 0)

# multiply the value of the likely critical habitat raster by 10 so it has different values to potential
likely_raster <- calc(likely_raster, fun=function(x){ x * 10})
plot(likely_raster)

# stack and then sum rasters
rs <- stack(likely_raster, potential_raster)
sum_raster <- calc(rs, sum)
plot(sum_raster)

# revalue areas that overlap (value of 11) to value for likely (10)
critical_habitat_basic_raster <- reclassify(sum_raster, cbind(10, Inf, 10))
plot(critical_habitat_basic_raster)

# write to raster
# writeRaster(critical_habitat_basic_raster, paste0(output_path, "Basic_Critical_Habitat_Raster.tif"), overwrite = TRUE)

rf<-writeRaster(critical_habitat_basic_raster, filename="Basic_Critical_Habitat_Raster.tif", datatype="INT1U", format="GTiff", overwrite=TRUE)



# Legend
# 0 = Unclassified
# 1 = Potential Critical Habitat
# 10 = Likely Critical Habitat

#OLD - make into shapefile for area calculations
critical_habitat_basic_polygon <- as.polygons(rast(critical_habitat_basic_raster))
critical_habitat_basic_polygon_moll <- st_transform(critical_habitat_basic_polygon, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
critical_habitat_basic_polygon_moll$area <- st_area(critical_habitat_basic_polygon_moll) * 1e-6
critical_habitat_basic_polygon <- st_make_valid(st_transform(critical_habitat_basic_polygon_moll, crs = 4326))

#NEW - make into shapefile for area calculations
critical_habitat_basic_polygon <- st_as_sf(as.polygons(rast(critical_habitat_basic_raster)))
critical_habitat_basic_polygon_moll <- st_transform(critical_habitat_basic_polygon, crs = 54009)
critical_habitat_basic_polygon_moll$area <- st_area(critical_habitat_basic_polygon_moll) * 1e-6
critical_habitat_basic_polygon <- st_make_valid(st_transform(critical_habitat_basic_polygon_moll, crs = 4326))

#NEW2 - make into shapefile for area calculations
critical_habitat_basic_raster <- rast(paste0(output_path, "Basic_Critical_Habitat_Raster.tif"))
critical_habitat_basic_polygon <- st_as_sf(as.polygons(critical_habitat_basic_raster))
critical_habitat_basic_polygon_moll <- st_transform(critical_habitat_basic_polygon, crs = 54009)
critical_habitat_basic_polygon_moll$area <- st_area(critical_habitat_basic_polygon_moll) * 1e-6
critical_habitat_basic_polygon <- st_make_valid(st_transform(critical_habitat_basic_polygon_moll, crs = 4326))

write_sf(critical_habitat_basic_polygon, paste0(output_path, "Basic_Critical_Habitat_Polygon.shp"))
plot(critical_habitat_basic_polygon)
