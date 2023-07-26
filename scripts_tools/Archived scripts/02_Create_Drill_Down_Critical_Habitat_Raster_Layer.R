#########################################################
#### CREATE DRILL DOWN CRITICAL HABITAT RASTER LAYER ####
#########################################################

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
library(arcgisbinding)
library(terra)
arc.check_product()

# set path variables (these will be the only lines that need changing in this script)
# path to where the GEE output shapefiles are stored
shapefile_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

# path to where you want the output saved
output_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/"

# read in shapefiles
likely <- read_sf(paste0(shapefile_path, "Likely_Critical_Habitat_Polygon.shp"))
potential <- read_sf(paste0(shapefile_path, "Potential_Critical_Habitat_Polygon.shp"))

# load example raster with appropriate extent (global) and resolution (1 km) properties (e.g. NatMod raster from the portal)
nat_mod <- arc.open('O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif')
raster <- as.raster(arc.raster(nat_mod))
raster

# create an empty raster with these properties
r <- raster(ext = extent(raster), resolution = res(raster))
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90)
plot(raster)

# combine to final
library("dplyr")
final_combined <- bind_rows(likely, potential)
final_combined_moll <- st_transform(final_combined, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
final_combined_moll <- final_combined_moll[, c("Type", "Feature", "C1", "C2", "C3", "C4", "C5", "geometry")]
final_combined_moll$area <- st_area(final_combined_moll) * 1e-6
final_combined <- st_make_valid(st_transform(final_combined_moll, crs = 4326))
         
# create lookup table to assign values
combined_values <- unique(paste(final_combined$Type, final_combined$Feature, sep="; "))
values <- 10^(1:length(combined_values))
lookup <- data.frame(combined_values, values) 
split <- as.data.frame(stringr::str_split_fixed(as.character(lookup$combined_values),'; ', n = 2))
colnames(split) <- c("Type", "Feature")
lookup <- merge(lookup, split, by=0, all=TRUE)
         
# join lookup table
final_combined <- final_combined %>% select(-"value") %>% inner_join(lookup)
         
# create raster
library(sf)
library(raster)
library(tidyverse)
drill_down_raster <- raster(fasterize(final_combined, r, field = "values", fun = "sum", background = 0))
drill_down_raster <- fasterize(final_combined, r, 'area')
plot(drill_down_raster)
plot(st_geometry(final_combined), add = TRUE)
         
# write files - shp
write_sf(final_combined_moll, paste0(output_path,"Drill_Down_Shapefile_Moll.shp"))
write_sf(final_combined, paste0(output_path,"Drill_Down_Shapefile.shp"))

# correct dateline
Drill_Down_Shapefile <- st_wrap_dateline(final_combined, options=c("WRAPDATELINE=TRUE","DATELINEOFFSET=50"))
Moll <- st_transform(Drill_Down_Shapefile,crs="Drill_Down_Shapefile_Moll.shp")

# write files - raster
writeRaster(drill_down_raster, paste0(output_path, "Drill_Down_Critical_Habitat_Raster.tif"), overwrite = TRUE)

# increase memory
memory.limit()
memory.limit(size=56000)

x <- rnorm(1000000000)

if(.Platform$OS.type=="windows")withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})

gc()
memory.limit(9999999999)
fit<-1m(Y~X)
gc()

save.image(file="temp.RData")
rm(list=Is())
load(file="temp.RData")

install.packages("disk.frame")
library(disk.frame)
setup_disk.frame()

