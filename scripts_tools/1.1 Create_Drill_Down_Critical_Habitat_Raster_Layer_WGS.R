################################################################
#### CREATE DRILL DOWN CRITICAL HABITAT RASTER LAYER in WGS ####
################################################################

# Author: Seb Dunnett
# Created: 16/02/2023
# Modified: 28/07/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,terra,tidyverse,units,tictoc, foreign)

# Turns off scientific notation, e.g. 12.0e+12
# Set terra to default to FLT8S (double float)
# Required to maintain accuracy of large numbers
options(scipen = 999)
terraOptions(datatype="FLT8S")

# load example raster with appropriate extent (global) and resolution (1/120 degrees ~ 1km)
raster_WGS <- rast(res=1/120)

cat("Importing and reprojecting shapefiles...\n")

# set path variables (these will be the only lines that need changing in this script)
# path to where the GEE output shapefiles are stored
scratch_path = "scratch/"

# path to where you want the output saved
output_path = "outputs/"

lookup = read.csv("scripts_tools/lookup.csv") %>% 
  arrange(Type,Feature)

# split the number of features into three roughly equal groups
# R loses precision over ~22 digits
# uncomment and run the next couple of lines to see an example
# options(scipen=0)
# 10^(1:40)
# options(scipen=999)
# 10^(1:40)
# this method can handle up to a max of ~60 triggers
ntriggers = nrow(lookup)
divs <- rep(ntriggers%/%3, 3)
mod <- ntriggers%%3
divs[seq_len(mod)] <- divs[seq_len(mod)] + 1

# we use 1, 3, and 5 to maintain information about overlaps
# position of number and number itself holds info
# e.g. 9 means that cell contains all three features at position one in their group (1+3+5=9)
# e.g. 600 means that cell contains two features at position three in their group (1+5=6)
values <- c(1,10^(1:(divs[1]-1)),
            3,3*(10^(1:(divs[2]-1))),
            5,5*(10^(1:(divs[3]-1))))
set <- rep(1:3, divs)

lookup$Values = values
lookup$Set = set
lookup$Type_Feature = paste0(lookup$Type,"; ",lookup$Feature)

################################################################
#### LIKELY ####################################################
################################################################

likely_polys_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^L_") & str_detect(.,"_polys.shp"))
likely_pts_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^L_") & str_detect(.,"_pts.shp"))

tic("read in likely polygons")
likely_polys = vect(lapply(paste0(scratch_path,likely_polys_files),vect)) %>% 
  terra::sort("Feature") %>% 
  terra::merge(select(lookup,Type,Feature,Values), all.x=TRUE, by.x=c('Type', 'Feature'), by.y=c('Type', 'Feature'))
toc()
likely_pts = vect(lapply(paste0(scratch_path,likely_pts_files),vect)) %>%
  terra::sort("Feature") %>% 
  terra::merge(select(lookup,Type,Feature,Values), all.x=TRUE, by.x=c('Type', 'Feature'), by.y=c('Type', 'Feature'))

correct_lpts_names = sapply(split(likely_pts,"Feature"), function(i) i[["Feature"]][1])

tic("rasterize likely points")
rasterize(likely_pts, raster_WGS, field="Values", by="Feature", fun=function(x) min(x,na.rm=TRUE), background=0, filename=paste0(scratch_path,"likely_WGS_pts.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S", names=correct_lpts_names))
toc()

correct_lpolys_names = sapply(split(likely_polys,"Feature"), function(i) i[["Feature"]][1])

tic("rasterize likely polygons")
rasterize(likely_polys, raster_WGS, field="Values", by="Feature", fun="min", touches=TRUE, background=0, filename=paste0(scratch_path,"likely_WGS_polys.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S",names=correct_lpolys_names))
toc()

likely_rfiles = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^L_") & str_detect(.,"WGS.tif"))

likely_rasters = rast(lapply(paste0(scratch_path,likely_rfiles),rast))

likely = c(rast(paste0(scratch_path,"likely_WGS_polys.tif")),
              rast(paste0(scratch_path,"likely_WGS_pts.tif")),
              likely_rasters)

#likely = likely[[order(names(likely))]]

#likely_vals = filter(lookup, Type=="Likely" & Feature %in% names(likely)) %>% 
#  pull(Values)

tic("reclass likely raster stack")
app(likely,sum,filename=paste0(scratch_path,"likely_sum.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S"))
toc()

################################################################
#### POTENTIAL #################################################
################################################################

potential_polys_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^P_") & str_detect(.,"_polys.shp"))
potential_pts_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^P_") & str_detect(.,"_pts.shp"))

tic("read in potential polygons")
potential_polys = vect(lapply(paste0(scratch_path,potential_polys_files),vect)) %>% 
  terra::sort("Feature") %>% 
  terra::merge(select(lookup,Type,Feature,Values), all.x=TRUE, by.x=c('Type', 'Feature'), by.y=c('Type', 'Feature'))
toc()
potential_pts = vect(lapply(paste0(scratch_path,potential_pts_files),vect)) %>% 
  terra::sort("Feature") %>% 
  terra::merge(select(lookup,Type,Feature,Values), all.x=TRUE, by.x=c('Type', 'Feature'), by.y=c('Type', 'Feature'))

correct_ppts_names = sapply(split(potential_pts,"Feature"), function(i) i[["Feature"]][1])

tic("rasterize potential points")
rasterize(potential_pts, raster_WGS, field="Values", by="Feature", fun=function(x) min(x,na.rm=TRUE), background=0, filename=paste0(scratch_path,"potential_WGS_pts.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S", names=correct_ppts_names))
toc()

correct_ppolys_names = sapply(split(potential_polys,"Feature"), function(i) i[["Feature"]][1])

tic("rasterize potential polygons")
rasterize(potential_polys, raster_WGS, field="Values", by="Feature", fun="min", touches=TRUE, background=0, filename=paste0(scratch_path,"potential_WGS_polys.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S",names=correct_ppolys_names))
toc()

potential_rfiles = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^P_") & str_detect(.,"WGS.tif"))

potential_rasters = rast(lapply(paste0(scratch_path,potential_rfiles),rast))

potential = c(rast(paste0(scratch_path,"potential_WGS_polys.tif")),
              rast(paste0(scratch_path,"potential_WGS_pts.tif")),
              potential_rasters)

#potential = potential[[order(names(potential))]]

#potential_vals = filter(lookup, Type=="Potential" & Feature %in% names(potential)) %>% 
#  pull(Values)

tic("reclass potential raster stack")
app(potential,sum,filename=paste0(scratch_path,"potential_sum.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S"))
toc()

# Combine

tic("combine likely and potential rasters")
combined = c(rast(paste0(scratch_path,"potential_sum.tif")),rast(paste0(scratch_path,"likely_sum.tif")))

app(combined, sum, filename=paste0(scratch_path,"likely_potential_sum.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S"))
toc()

# combine potential and likely to final
final_combined_WGS <- rbind(likely_WGS, potential_WGS)

final_combined_moll <- rbind(likely_moll, potential_moll)

# use Mollweide equal area to calculate areas
final_combined_WGS$Area <- st_area(final_combined_moll) %>% units::set_units(km2) %>% units::drop_units()
final_combined_moll$Area <- st_area(final_combined_moll) %>% units::set_units(km2) %>% units::drop_units()

cat("Creating lookup table...\n")

# combine type and feature to one variable, e.g. "Likely; Tiger Conservation Landscapes"
combined_values <- unique(paste(final_combined_WGS$Type, final_combined_WGS$Feature, sep="; ")) %>% 
  sort

# split the number of features into three roughly equal groups
# R loses precision over ~22 digits
# uncomment and run the next couple of lines to see an example
# options(scipen=0)
# 10^(1:40)
# options(scipen=999)
# 10^(1:40)
# this method can handle up to a max of ~60 triggers
ntriggers = length(combined_values)
divs <- rep(ntriggers%/%3, 3)
mod <- ntriggers%%3
divs[seq_len(mod)] <- divs[seq_len(mod)] + 1

# we use 1, 3, and 5 to maintain information about overlaps
# position of number and number itself holds info
# e.g. 9 means that cell contains all three features at position one in their group (1+3+5=9)
# e.g. 600 means that cell contains two features at position three in their group (1+5=6)
values <- c(1,10^(1:(divs[1]-1)),
            3,3*(10^(1:(divs[2]-1))),
            5,5*(10^(1:(divs[3]-1))))
set <- rep(1:3, divs)

# create lookup
lookup <- data.frame(combined_values, values, set) %>% 
  separate(combined_values, sep = "; ", into = c("Type","Feature"), remove = FALSE) %>% 
  left_join(st_drop_geometry(final_combined_WGS), by = c("Type","Feature"))

# join lookup table to shapefiles to assign values
final_combined_WGS <- final_combined_WGS %>%
  inner_join(dplyr::select(lookup,-C1:-C5), by = c("Type","Feature"))

final_combined_moll <- final_combined_moll %>%
  inner_join(dplyr::select(lookup,-C1:-C5), by = c("Type","Feature"))

cat("Rasterizing shapefiles...\n")

# first sum within a group to calculate overlaps within that group
# e.g. 1001010 or 5055500
drill_down_raster_WGS_set1 <- fasterize(filter(final_combined_WGS,set==1), raster_WGS,
                                         field = "values",
                                         fun = "sum",
                                         background = 0)

drill_down_raster_WGS_set2 <- fasterize(filter(final_combined_WGS,set==2), raster_WGS,
                                         field = "values",
                                         fun = "sum",
                                         background = 0)

drill_down_raster_WGS_set3 <- fasterize(filter(final_combined_WGS,set==3), raster_WGS,
                                         field = "values",
                                         fun = "sum",
                                         background = 0)

# stack rasters and sum across to also calculate overlaps between groups
# e.g. 3095400
# datatype "FLT8S" required to maintain precision
stack_WGS = stack(drill_down_raster_WGS_set1,
              drill_down_raster_WGS_set2,
              drill_down_raster_WGS_set3)

cat("Summing raster stack to calculate overlaps...\n")

rs_WGS = raster::calc(stack_WGS,sum,datatype="FLT8S")

################################################################################
# RAT SETUP ####################################################################   
################################################################################

cat("Extracting unique raster values...\n")

# extract unique values from raster
# these are all unique combinations of triggers at 1km resolution
ID = unique(rast(paste0(scratch_path,"likely_potential_sum.tif"))) %>% 
  rename(ID=sum)

cat("Creating raster attribute table...\n")

# separate each unique ID value into digits across columns
rat = ID %>%
  separate(1, into = paste0("V",0:max(divs)), sep="", fill="left") %>% 
  mutate(across(everything(), ~ na_if(.x,""))) %>% 
  mutate(across(everything(), ~ replace(.x, is.na(.x), 0))) %>% 
  dplyr::select(-V0)

# these are the values that could exist for a trigger in each group
# e.g. a 6 could only be 1+5
vals_lookup = setNames(list(c(1,4,6,9),
                          c(3,4,8,9),
                          c(5,6,8,9)),
                     1:3)

# function iterates through the three original groups of triggers
# places a 1 if there is an overlap for this group, otherwise 0
rats = lapply(1:3, function(x){
  
  vals = vals_lookup[[x]] %>% as.character
  
  rat_out = rat %>% 
    mutate(across(everything(), ~ ifelse(.x %in% vals, 1, 0)))
  
  rat_out = rat_out[,(1+(ncol(rat_out) - divs[x])):ncol(rat_out)]
  
  names(rat_out) = filter(lookup,Set==x) %>% pull(Type_Feature) %>% rev
  
  rat_out = cbind(ID=ID, rat_out)
  
  rat_out
  
})

# combines all groups into one wide data frame
rat_full = rats %>% reduce(full_join, by = "ID")

# pivot longer so we have one row for each trigger and ID
add_cr = rat_full %>%
  pivot_longer(2:42, names_to = "Type_Feature", values_to = "Join")

# add value = 1 to lookup so we only add criteria where a cell has been triggered
# add 10 for likely, 1 for potential
cr_df = mutate(lookup, Join=1) %>%
  dplyr::select(Join,Type_Feature,C1:C5) %>% 
  mutate(CH = rep(c(10,1),c(table(lookup$Type)[["Likely"]],table(lookup$Type)[["Potential"]])))

# join to pivoted attribute table, replacing NAs (no lookup value)  with 0  
add_cr = left_join(add_cr,cr_df,by=c("Join","Type_Feature")) %>% 
  mutate(across(everything(), ~ replace(.x, is.na(.x), 0)))

# group by ID, collapsing to one row per ID
cr_rat = add_cr %>% 
  group_by(ID) %>% 
  summarise(CH = ifelse(any(CH == 10), 10, ifelse(any(CH == 1), 1, 0)),
            C1 = ifelse(sum(C1) > 0, 1, 0),
            C2 = ifelse(sum(C2) > 0, 1, 0),
            C3 = ifelse(sum(C3) > 0, 1, 0),
            C4 = ifelse(sum(C4) > 0, 1, 0),
            C5 = ifelse(sum(C5) > 0, 1, 0))

# join criteria to output attribute table
final_rat = inner_join(cr_rat,rat_full,by="ID")

cat("Calculating cell frequencies...\n")

# ArcGIS calculates this automatically but good to have for software independence
cell_counts = freq(rast(paste0(scratch_path,"likely_potential_sum.tif")))

# Replace IDs with a more sensible value so we can save as integer raster
# e.g. 5003010, 500, 95060601 --> 1,2,3 etc.
final_rat = final_rat %>%  
  mutate(Value = 1:nrow(final_rat),
         Count = cell_counts$count,
         .after = ID) %>% 
  dplyr::select(-ID)

# ArcGIS needs shortened field names
# match short name from lookup and replace RAT names
shorts = lookup$Short[match(names(final_rat),lookup$Type_Feature)] %>% discard(is.na(.))

names(final_rat) <- c("VALUE","COUNT","CH","C1","C2","C3","C4","C5",shorts)

cat("Reclassifying raster...\n")

# reclassify output raster to our more sensible values
rss = classify(rast(paste0(scratch_path,"likely_potential_sum.tif")),as.matrix(cbind(sort(ID$ID),1:nrow(ID))))

# saving files and removing previous versions

rst_file = paste0(output_path,"Critical_Habitat_Drill_Down_WGS.tif")

cat("Saving raster...\n")

if(file.exists(rst_file)){
  cat("Remove or archive previous version of raster file")
} else{
  writeRaster(rss,
              filename = rst_file,
              datatype = "INT2U")
}

cat("Saving RAT...\n")

dbf_file = paste0(output_path,"Critical_Habitat_Drill_Down_WGS.tif.vat.dbf")

if(file.exists(dbf_file)){
  cat("Remove or archive previous version of raster attribute table")
} else{
  foreign::write.dbf(as.data.frame(final_rat),
                     file = dbf_file)
}

cat("Script complete: ")

toc()