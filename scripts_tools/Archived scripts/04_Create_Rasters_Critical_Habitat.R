#########################################################
#### CREATE INPUT RASTER LAYERS FOR CRITICAL HABITAT ####
#########################################################

# example shown for coral reef layers

# Install packages (if required)
list.of.packages <- c("sf", "fasterize", "raster", "arcgisbinding", "rgeos", "remotes", "stars")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
remotes::install_github("rspatial/terra")

# Load packages
library(sf)
library(fasterize)
library(raster)
library(rgeos)
library(arcgisbinding)
library(terra)
library(stars)
arc.check_product()

# set path variables (these will be the only lines that need changing in this script)
# path to where the GEE output shapefiles are stored
shapefile_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

# path to where you want the output saved
output_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/"

#### Coral Reefs Poly ####
coral <- arc.open('https://data-portal.internal-gis.unep-wcmc.org/server/rest/services/WCMC008_CoralReef/FeatureServer/1')
coral_poly <- arc.select(coral)
coral_poly_sf <- st_make_valid(arc.data2sf(coral_poly))
coral_poly_sf <- st_as_sf(gUnaryUnion(as_Spatial(coral_poly_sf)))
coral_poly_sf <- st_transform(coral_poly_sf, crs = 4326)
plot(st_geometry(coral_poly_sf))

#### Coral Reefs Point ####
coral_pnt <- arc.open('https://data-portal.internal-gis.unep-wcmc.org/server/rest/services/WCMC008_CoralReef/FeatureServer/0')
coral_pnt <- arc.select(coral_pnt)
coral_pnt_sf <- st_make_valid(arc.data2sf(coral_pnt))
coral_pnt_sf <- st_buffer(coral_pnt_sf, 0.1) # need to add buffer to make POLYGON
coral_pnt_sf <- st_as_sf(gUnaryUnion(as_Spatial(coral_pnt_sf)))
coral_pnt_sf <- st_transform(coral_pnt_sf, crs = 4326)
plot(st_geometry(coral_pnt_sf))

# load example raster with appropriate extent (global) and resolution (1 km) properties (e.g. NatMod raster from the portal)
nat_mod <- arc.open('https://data-gis.unep-wcmc.org/server/rest/services/NatMod_Screening_Layer/ImageServer')
raster <- as.raster(arc.raster(nat_mod))
raster

# create an empty raster with these properties
r <- raster(ext = extent(raster), res = res(raster))
r_pnt <- raster(ext = extent(coral_pnt_sf))
r_poly <- raster(ext = extent(coral_poly_sf))

# convert polygons and points to raster
poly_raster <- rasterize(vect(coral_poly_sf), rast(r_poly), background = 0, touches=TRUE, small = TRUE)
plot(poly_raster)

point_raster <- rasterize(vect(coral_pnt_sf), rast(r_pnt), background = 0, touches=TRUE, small = TRUE)
plot(point_raster)


# resample rasters
point_raster_resample <- terra::resample(point_raster, rast(r), method="bilinear")
plot(point_raster_resample)
poly_raster_resample <- terra::resample(poly_raster, rast(r), method="bilinear")
plot(poly_raster_resample)

# sum layers
sum <- sum(c(point_raster_resample, poly_raster_resample), na.rm = TRUE)
plot(sum)

# reclassify
reclass <- classify(sum, cbind(0.000000000000001, Inf, 1))
plot(reclass)

# write to raster
writeRaster(reclass, paste0(output_path, "L_C4_coralreef.tif"), overwrite = TRUE)

