####################################################
#### CREATE BASIC CRITICAL HABITAT RASTER LAYER ####
####################################################

# This script can be run once the polygon files have been exported from Google Earth Engine
# NOTE: needs > 18 GB of memory to run
# These scripts can be found:
# Potential: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FPotential_Critical_Habitat
# Likely: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FLikely_Critical_Habitat

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,raster,fasterize,tidyverse,rgeos,terra,units,tictoc)

tic()
# set path variables (these will be the only lines that need changing in this script)
# path to where the GEE output shapefiles are stored
shapefile_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

# path to where you want the output saved
output_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/"

# read in WGS shapefiles
likely_WGS <- st_read(paste0(shapefile_path, "Likely_Critical_Habitat_Polygon.shp"))
potential_WGS <- st_read(paste0(shapefile_path, "Potential_Critical_Habitat_Polygon.shp"))

# create Mollweide shps
# need to wrap dateline to avoid projection error
likely_moll <- st_wrap_dateline(likely_WGS,
                                options = c("WRAPDATELINE=TRUE","DATELINEOFFSET=90")) %>% 
  st_transform("ESRI:54009") %>% 
  st_cast("MULTIPOLYGON")

potential_moll <- st_wrap_dateline(potential_WGS,
                                options = c("WRAPDATELINE=TRUE","DATELINEOFFSET=90")) %>% 
  st_transform("ESRI:54009") %>% 
  st_cast("MULTIPOLYGON")

# load example raster with appropriate extent (global) and resolution (1 km) properties (e.g. NatMod raster from the portal)
raster_WGS <- raster('O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif')
raster_WGS

# create example raster in Mollweide with global extent and 1km resolution
raster_moll <- raster(crs=st_crs("ESRI:54009")$proj4string,
                      res=1000,
                      ext=extent(c(-18040095.7,18040095.7,-9020047.85,9020047.85)))
raster_moll

# convert polygons to raster
# making a new field in polys to give to rasters
likely_raster_WGS <- fasterize(likely_WGS %>% mutate(value=10),
                               raster_WGS,
                               field = "value")
potential_raster_WGS <- fasterize(potential_WGS, raster_WGS, background = 0)

likely_raster_moll <- fasterize(likely_moll %>% mutate(value=10),
                                raster_moll,
                                field = "value")
potential_raster_moll <- fasterize(potential_moll, raster_moll, background = 0)

# use mask to update overlap values
critical_habitat_basic_raster_moll = mask(x=potential_raster_moll,
                                          mask=likely_raster_moll,
                                          updatevalue=10,
                                          inverse=TRUE)

critical_habitat_basic_raster_WGS = mask(x=potential_raster_WGS,
                                          mask=likely_raster_WGS,
                                          updatevalue=10,
                                          inverse=TRUE)

# plot results
# Legend
# 0 = Unclassified
# 1 = Potential Critical Habitat
# 10 = Likely Critical Habitat
plot(critical_habitat_basic_raster_WGS)
plot(critical_habitat_basic_raster_moll)

# write to raster
writeRaster(critical_habitat_basic_raster_WGS, paste0(output_path, "Basic_Critical_Habitat_Raster.tif"), overwrite = TRUE)
potential_area = freq(critical_habitat_basic_raster_moll,value=1)
likely_area = freq(critical_habitat_basic_raster_moll,value=10)
critical_habitat_area = units::set_units(likely_area + potential_area,km2)
critical_habitat_area
toc()
