################################################################
#### CREATE DRILL DOWN CRITICAL HABITAT RASTER LAYER in WGS ####
################################################################

# Author: Seb Dunnett
# Created: 16/02/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,raster,fasterize,tidyverse,rgeos,units,tictoc,foreign)

# Turns off scientific notation, e.g. 12.0e+12
# Required to maintain accuracy of large numbers
options(scipen = 999)

tic()

cat("Importing and reprojecting shapefiles...\n")

# set path variables (these will be the only lines that need changing in this script)
# path to where the GEE output shapefiles are stored
scratch_path = "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

# path to where you want the output saved
output_path = "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/"

# read in WGS shapefiles
likely_WGS = st_read(paste0(scratch_path, "Likely_Critical_Habitat_vectors.gpkg"), quiet=TRUE)
potential_WGS = st_read(paste0(scratch_path, "Potential_Critical_Habitat_vectors.gpkg"), quiet=TRUE)

likely_WGS_pts = filter(likely_WGS, st_geometry_type(likely_WGS)=="MULTIPOINT")
likely_WGS_polys = filter(likely_WGS, st_geometry_type(likely_WGS)=="MULTIPOLYGON")

potential_WGS_pts = filter(potential_WGS, st_geometry_type(potential_WGS)=="MULTIPOINT")
potential_WGS_polys = filter(potential_WGS, st_geometry_type(potential_WGS)=="MULTIPOLYGON")

L_C1_IUCN_CR_D = st_read(paste0(scratch_path,"L_C1_IUCN_CR_D.gpkg"), quiet=TRUE)
L_C1_IUCN_EN_D = st_read(paste0(scratch_path,"L_C1_IUCN_EN_D.gpkg"), quiet=TRUE)
P_C1_IUCN_VU_D2 = st_read(paste0(scratch_path,"P_C1_IUCN_VU_D2.gpkg"), quiet=TRUE)

likely_WGS_polys = rbind(likely_WGS_polys,L_C1_IUCN_CR_D,L_C1_IUCN_EN_D)
potential_WGS_polys = rbind(potential_WGS_polys,P_C1_IUCN_VU_D2)

# create Mollweide vectors
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
# datatype "FLT8S" required to maintain precision
raster_WGS <- raster('O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif')
dataType(raster_WGS) <- "FLT8S"
raster_WGS

# create example raster in Mollweide with global extent and 1km resolution
# datatype "FLT8S" required to maintain precision
raster_moll <- raster(crs=st_crs("ESRI:54009")$proj4string,
                      res=1000,
                      ext=extent(c(-18040095.7,18040095.7,-9020047.85,9020047.85)))
dataType(raster_moll) <- "FLT8S"
raster_moll

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
ID = unique(values(rs_WGS))

cat("Creating raster attribute table...\n")

# separate each unique ID value into digits across columns
rat = data.frame(ID) %>%
  separate(ID, into = paste0("V",0:max(divs)), sep="", fill="left") %>% 
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
  
  names(rat_out) = filter(lookup,set==x) %>% pull(combined_values) %>% rev
  
  rat_out = cbind(ID=ID, rat_out)
  
  rat_out
  
})

# combines all groups into one wide data frame
rat_full = rats %>% reduce(full_join, by = "ID")

# pivot longer so we have one row for each trigger and ID
add_cr = rat_full %>%
  pivot_longer(2:42, names_to = "combined_values", values_to = "value")

# add value = 1 to lookup so we only add criteria where a cell has been triggered
# add 10 for likely, 1 for potential
cr_df = mutate(lookup, value=1) %>%
  dplyr::select(value,combined_values,C1:C5) %>% 
  mutate(CH = rep(c(10,1),c(table(lookup$Type)[["Likely"]],table(lookup$Type)[["Potential"]])))

# join to pivoted attribute table, replacing NAs (no lookup value)  with 0  
add_cr = left_join(add_cr,cr_df,by=c("value","combined_values")) %>% 
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
cell_counts = freq(rs_WGS)[,2]

# Replace IDs with a more sensible value so we can save as integer raster
# e.g. 5003010, 500, 95060601 --> 1,2,3 etc.
final_rat = final_rat %>%  
  mutate(Value = 1:nrow(final_rat),
         Count = cell_counts,
         .after = ID) %>% 
  dplyr::select(-ID)

# ArcGIS needs shortened field names
# lookup short name from csv and replace RAT names
fid_lookup = read.csv("O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scripts_tools/FID_LOOKUP.csv")
shorts = fid_lookup$SHORTNAME[match(names(final_rat),fid_lookup$LONGNAME)] %>% discard(is.na(.))

names(final_rat) <- c("VALUE","COUNT","CH","C1","C2","C3","C4","C5",shorts)

cat("Reclassifying raster...\n")

# reclassify output raster to our more sensible values
rss = reclassify(rs_WGS,as.matrix(cbind(sort(ID),1:length(ID))),datatype="INT2U")

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