###########################################################
#### CREATE DRILL-DOWN CRITICAL HABITAT RASTER POLYGON ####
###########################################################

# This script can be run once the polygon files have been exported from Google Earth Engine
# These scripts can be found:
# Potential: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FPotential_Critical_Habitat
# Likely: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FLikely_Critical_Habitat

# Install packages (if required)
list.of.packages <- c("sf", "fasterize", "raster", "arcgisbinding", "rmapshaper", "rgeos", "data.table", "mapedit")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(sf)
library(raster)
library(rmapshaper)
library(rgeos)
library(data.table)
library(tidyverse)
library(arcgisbinding)
library(mapedit)
arc.check_product()

# set path variables (these will be the only lines that need changing in this script)
# path to where the GEE output shapefiles are stored
shapefile_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

# path to where you want the output saved
output_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/"

# read in shapefiles
likely <- read_sf(paste0(shapefile_path, "Likely_Critical_Habitat_Polygon.shp"))
potential <- read_sf(paste0(shapefile_path, "Potential_Critical_Habitat_Polygon.shp"))

# take a slice for testing
# likely <- likely %>% group_by(Feature) %>% slice_sample(n = 200)
# potential <- potential %>% group_by(Feature) %>% slice_sample(n = 200)

# bind polygons
ch_bind <- st_as_sf(raster::bind(as_Spatial(likely), as_Spatial(potential)))

# identify duplicate polygons
ch_bind <- ch_bind[duplicated(ch_bind$geometry),]

# get unique geometries
geometry_list <- unique(ch_bind$geometry)

# create empty lists
old_polygon_list <- list()
new_polygon_list <- list()

# run loop to combine attributes of identical polygons
for (i in 1:length(geometry_list)) {
  
  print(i)
  polygons <- ch_bind[ch_bind$geometry %in% geometry_list[i],]
  old_polygon_list[[i]] <- polygons
  features <- paste(polygons$Feature, collapse = '; ')
  C1 <- ifelse(sum(polygons$C1) > 1, 1, 0)
  C2 <- ifelse(sum(polygons$C2) > 1, 1, 0)
  C3 <- ifelse(sum(polygons$C3) > 1, 1, 0)
  C4 <- ifelse(sum(polygons$C4) > 1, 1, 0)
  C5 <- ifelse(sum(polygons$C5) > 1, 1, 0)
  Type <- ifelse(grepl("Likely", paste(polygons$Type, collapse = '; '), fixed=TRUE) == TRUE, "Likely", "Potential")
  new_poly <- polygons[1,]
  new_poly$C1 <- C1
  new_poly$C2 <- C2
  new_poly$C3 <- C3
  new_poly$C4 <- C4
  new_poly$C5 <- C5
  new_poly$Type <- Type
  new_poly$Feature <- features
  new_polygon_list[[i]] <- new_poly
  
}

# combine lists into one
new_polygons <- mapedit:::combine_list_of_sf(new_polygon_list)
old_polygons <- mapedit:::combine_list_of_sf(old_polygon_list)

# update shapefiles
likely_old <- old_polygons %>% filter(Type == "Likely")
likely_remove_old <- likely %>% anti_join(as.data.frame(likely_old))
likely_new <- new_polygons %>% filter(Type == "Likely")
likely_updated <- bind_rows(likely_remove_old, likely_new)

# update shapefiles
potential_old <- old_polygons %>% filter(Type == "Potential")
potential_remove_old <- potential %>% anti_join(as.data.frame(potential_old))
potential_new <- new_polygons %>% filter(Type == "Potential")
potential_updated <- bind_rows(potential_remove_old, potential_new)

# project to moll
likely_updated_moll <- st_transform(likely_updated, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
potential_updated_moll <- st_transform(potential_updated, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

test_poly_list <- list()
overlap_polygon_list <- list()

for (i in 1:nrow(likely_updated_moll)){
  print(i)
  poly <- likely_updated_moll[i,]
  intersect <- st_intersects(poly, potential_updated_moll, sparse =  FALSE) 
  which <- which(intersect) 
  overlap <- potential_updated_moll[which,]
  ifelse(nrow(overlap) == 0, print("No overlap"), overlap_polygon_list[[i]] <- overlap)
  ifelse(nrow(overlap) == 0, print("No overlap"), test_poly_list[[i]] <- poly)
  
}


keep <- which(sapply(overlap_polygon_list, is.null) == FALSE)
test_poly_list_keep <- test_poly_list[keep]
overlap_polygon_list <- overlap_polygon_list[keep]


intersection_list <- list()
difference_list <- list()

for (i in 1:length(test_poly_list_keep)){
  
  print(i)
  poly <- test_poly_list_keep[[i]]
  overlaps <- overlap_polygon_list[[i]]
  
  stintersection <- st_intersection(st_make_valid(st_buffer(poly, dist = 0)), st_make_valid(st_buffer(overlaps, dist = 0)))
  stintersection$Feature <- str_c(stintersection$Feature,"; ", stintersection$Feature.1)
  stintersection$C1 <- ifelse(stintersection$C1 + stintersection$C1.1 > 0, 1, 0)
  stintersection$C2 <- ifelse(stintersection$C2 + stintersection$C2.1 > 0, 1, 0)
  stintersection$C3 <- ifelse(stintersection$C3 + stintersection$C3.1 > 0, 1, 0)
  stintersection$C4 <- ifelse(stintersection$C4 + stintersection$C4.1 > 0, 1, 0)
  stintersection$C5 <- ifelse(stintersection$C5 + stintersection$C5.1 > 0, 1, 0)
  stintersection <- stintersection %>% select(c("C3","C4","Type", "C5","count","value","Feature","C1","C2"))
  ifelse(st_geometry_type(stintersection) == "POINT", print("Not a Polygon"), stintersection_poly <- st_collection_extract(stintersection, "POLYGON"))
  
  stdifference <- st_difference(st_make_valid(st_buffer(overlaps, dist = 0)), st_make_valid(st_buffer(poly, dist = 0)))
  stdifference <- stdifference %>% select(c("C3","C4","Type", "C5","count","value","Feature","C1","C2"))
  stdifference_poly <- st_cast(stdifference, "POLYGON")
  
  intersection_list[[i]] <- stintersection_poly
  difference_list[[i]] <- stdifference_poly
  
  stintersection_poly <- NULL
  
}

keep2 <- which(sapply(intersection_list, is.null) == FALSE)
intersection_list <- intersection_list[keep2]

# combine lists into one
intersection_polygons <- mapedit:::combine_list_of_sf(intersection_list)
difference_polygons <- mapedit:::combine_list_of_sf(difference_list)
remove_likely_polygons <- rbindlist(test_poly_list_keep)
remove_potential_polygons <- rbindlist(overlap_polygon_list)

# update shapefiles
likely_remove_old_v2 <- likely_updated_moll %>% anti_join(remove_likely_polygons) 
likely_updated_moll_v2 <- bind_rows(likely_remove_old_v2, intersection_polygons)

# update shapefiles
potential_remove_old_v2 <- potential_updated_moll %>% anti_join(remove_potential_polygons) 
potential_updated_moll_v2 <- bind_rows(potential_remove_old_v2, difference_polygons)

# quick check for duplicates
likely_updated_moll_v2 <- unique(likely_updated_moll_v2)
potential_updated_moll_v2 <- unique(potential_updated_moll_v2)

# combine to final
final_combined_moll <- bind_rows(likely_updated_moll_v2, potential_updated_moll_v2)
final_combined_moll <- final_combined_moll[, c("Type", "Feature", "C1", "C2", "C3", "C4", "C5", "count", "value", "geometry")]
final_combined_moll$area <- st_area(final_combined_moll) * 1e-6
final_combined <- st_make_valid(st_transform(final_combined_moll, crs = 4326))

# write file
write_sf(final_combined_moll, "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/CH_combined_shapefile_moll.shp")
write_sf(final_combined, "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/CH_combined_shapefile.shp")

# useful
# https://github.com/r-spatial/sf/issues/1230
i = st_intersection(potential_WGS)
i$ids = sapply(i$origins, function(x) ifelse(sum(i$C4[x])>1,1,0))
i$ids