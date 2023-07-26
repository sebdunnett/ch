#########################################################
#### CREATE DRILL DOWN CRITICAL HABITAT RASTER LAYER ####
#########################################################

# This script can be run once the polygon files have been exported from Google Earth Engine
# NOTE: needs > 18 GB of memory to run
# These scripts can be found:
# Potential: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FPotential_Critical_Habitat
# Likely: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FLikely_Critical_Habitat

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,tidyverse,units,tictoc)

tic("drill down")

# set path variables (these will be the only lines that need changing in this script)
# path to where the GEE output shapefiles are stored
shapefile_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

# path to where you want the output saved
output_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/"

# read in WGS shapefiles
likely <- st_read(paste0(shapefile_path, "Likely_Critical_Habitat_Polygon.shp")) %>% 
  mutate(troubleshoot=1:nrow(.))
potential <- st_read(paste0(shapefile_path, "Potential_Critical_Habitat_Polygon.shp")) %>% 
  mutate(troubleshoot=(nrow(likely)+1):(nrow(likely) + nrow(.)))

# take a slice for testing
likely_sample <- likely %>% slice_sample(n = 10000)
potential_sample <- potential %>% slice_sample(n = 10000)

# bind polygons
ch_bind <- rbind(likely_sample,potential_sample)

tic("removing duplicates")
ch_bind <- ch_bind %>% 
  group_by(geometry) %>% 
  summarise(C1 = ifelse(sum(C1) > 1, 1, 0),
            C2 = ifelse(sum(C2) > 1, 1, 0),
            C3 = ifelse(sum(C3) > 1, 1, 0),
            C4 = ifelse(sum(C4) > 1, 1, 0),
            C5 = ifelse(sum(C5) > 1, 1, 0),
            Type = ifelse(grepl("Likely", paste(Type, collapse = '; '), fixed=TRUE) == TRUE, "Likely", "Potential"),
            Features = paste(Feature, collapse = '; '),
            troubleshoot = paste(troubleshoot, collapse = '; '),
            geometry = st_union(geometry))
toc()

ch_bind$ix_id <- st_intersects(ch_bind)
ch_bind$ix <- map_int(ch_bind$ix_id,length)

tic("intersects")
test = invisible(plyr::ldply(1:nrow(ch_bind),function(x){
  p = ch_bind[x,]
  ix = ch_bind[[x,"ix"]]
  
  if(x==nrow(ch_bind)){
    cat("finished")
  } else{
    cat(paste0(x,"..."))
  }
  
  if(ix>1){
    ix_ids = ch_bind[[x,"ix_id"]][[1]]
    i = st_intersection(p,ch_bind[ix_ids,])
    v = st_is_valid(i)  
  
    valid = ifelse(sum(v)<nrow(i),FALSE,TRUE)

    vv = st_make_valid(i) %>% 
      st_is_valid()
    
    post_valid = ifelse(sum(vv)<nrow(i),FALSE,TRUE)
    
    rtn = data.frame(id=x,valid=valid,post_valid=post_valid)
    
  } else{
    rtn = data.frame(id=x,valid=NA,post_valid=NA)
  }
  
  return(rtn)
  
}))
toc()

# useful
# https://github.com/r-spatial/sf/issues/1230
tic()
ch_intersect = ch_bind %>% 
  st_buffer(0) %>%
  st_make_valid %>%
  st_intersection
toc()

ch_intersect$ids = sapply(i$origins, function(x) ifelse(sum(i$C4[x])>0,1,0))
i$ids






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

lapply(8:20,function(x){
poly = ch_bind[invalid_ids[x],]
other_polys = ch_bind[ch_bind[[invalid_ids[x],"ix_id"]][[1]],]
all_polys = rbind(poly,other_polys)
i = st_intersection(all_polys) %>% 
  mutate(unq = 1:nrow(.))
a = ggplot(all_polys, aes(fill=as.factor(troubleshoot)),alpha=0.5) +
  geom_sf() + ggtitle("original polys") + theme_void() + theme(legend.position = "none")
b = ggplot(i) + geom_sf() + facet_wrap(~unq) + ggtitle("intersected polys") + theme_void()
c = ggplot(st_make_valid(i)) + geom_sf() + facet_wrap(~unq) + ggtitle("intersected polys made valid") + theme_void()

cowplot::plot_grid(a,NULL,b,c,nrow=2)
})
