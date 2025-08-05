#########################################################
#### CREATE DRILL DOWN CRITICAL HABITAT RASTER LAYER ####
#########################################################

# Author: Seb Dunnett
# Created: 16/02/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,terra,tidyverse,units,tictoc,foreign)

# Turns off scientific notation, e.g. 12.0e+12
# Set terra to default to FLT8S (double float)
# Required to maintain accuracy of large numbers
options(scipen = 999)
terraOptions(datatype="FLT8S", memfrac=0.8)

# import helper functions
source("scripts_tools/0 spatial_processing_functions.R")

tic("time to complete")

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

raster_WGS = rast(res=1/120)

################################################################
#### LIKELY ####################################################
################################################################

cat("read in likely points and polygons\n")

# list all polygon files in scratch folder
likely_polys_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^L_") & str_detect(.,"_polys.shp"))
# list all multipoint files in scratch folder
likely_pts_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^L_") & str_detect(.,"_pts.shp"))

# read in likely polygons
# merge with values we need to transfer when rasterising
tic("time to read in likely polygons")
likely_polys = vect(lapply(paste0(scratch_path,likely_polys_files),vect)) %>%
  terra::sort("Feature") %>% 
  terra::merge(select(lookup,Type,Feature,Values), all.x=TRUE, by.x=c('Type', 'Feature'), by.y=c('Type', 'Feature'))
toc()

# read in likely multipoints
# merge with values we need to transfer when rasterising
likely_pts = vect(lapply(paste0(scratch_path,likely_pts_files),vect)) %>%
  terra::sort("Feature") %>% 
  terra::merge(select(lookup,Type,Feature,Values), all.x=TRUE, by.x=c('Type', 'Feature'), by.y=c('Type', 'Feature'))

# terra::rasterize does not currently always correctly transfer names when rasterising
# this is a safeguard against it
correct_lpts_names = sapply(split(likely_pts,"Feature"), function(i) i[["Feature"]][1])

cat("rasterising polygons and points\n")

# rasterise likely multipoints
# transfer values for binary info retention when summing features
tic("rasterise likely points")
rasterize(likely_pts, raster_WGS, field="Values", by="Feature", fun=function(x) min(x,na.rm=TRUE), background=0, filename=paste0(scratch_path,"likely_pts.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S", names=correct_lpts_names))
toc()

# terra::rasterize does not currently always correctly transfer names when rasterising
# this is a safeguard against it
correct_lpolys_names = sapply(split(likely_polys,"Feature"), function(i) i[["Feature"]][1])

# rasterise likely polygons
# transfer values for binary info retention when summing features
tic("rasterise likely polygons")
rasterize(likely_polys, raster_WGS, field="Values", by="Feature", fun="min", touches=TRUE, background=0, filename=paste0(scratch_path,"likely_polys.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S",names=correct_lpolys_names))
toc()

cat("importing already rasterised input data\n")

# some input data are already rasterised
# list files, import and stack
# order layers alphabetically and transfer values for binary info retention when summing features
likely_rfiles = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^L_") & str_detect(.,".tif") & !str_detect(.,"_uncertainty|_old"))
likely_rasters = rast(lapply(paste0(scratch_path,likely_rfiles),rast))
likely_rasters = likely_rasters[[order(names(likely_rasters))]]
tic("reclass input rasters")
likely_rasters = likely_rasters*(lookup[lookup$Feature %in% names(likely_rasters),"Values"])
toc()

# stack rasterised multipoints, polygons and existing rasters
likely = c(rast(paste0(scratch_path,"likely_polys.tif")),
              rast(paste0(scratch_path,"likely_pts.tif")),
              likely_rasters)

# features occasionally count as both likely and potential
# these lines ensure they are handled correctly in both categories
likely_duplicates = duplicated(names(likely)) | duplicated(names(likely),fromLast=TRUE)

if(any(likely_duplicates)){
  likely_duplicate_stack = likely[[likely_duplicates]]
  
  likely_unique_stack = likely[[!likely_duplicates]]
  
  likely_duplicate_stack = rast(lapply(unique(names(likely_duplicate_stack)),function(x){
    omah = which(x==names(likely_duplicate_stack))
    lay = likely_duplicate_stack[[omah]] |>
      app(max)
    names(lay) = x
    return(lay)
  }))
  
  likely = c(likely_unique_stack,likely_duplicate_stack)
} else{}

# sum raster layers to output single raster with info retained on features
cat("summing likely raster stack\n")
tic("summing likely raster stack")
app(likely,sum,filename=paste0(scratch_path,"likely_sum.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S"))
toc()

################################################################
#### POTENTIAL #################################################
################################################################

cat("read in potential points and polygons\n")

# list all polygon files in scratch folder
potential_polys_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^P_") & str_detect(.,"_polys.shp"))

# list all multipoint files in scratch folder
potential_pts_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^P_") & str_detect(.,"_pts.shp"))

# read in potential polygons
# merge with values we need to transfer when rasterising
tic("read in potential polygons")
potential_polys = vect(lapply(paste0(scratch_path,potential_polys_files),vect)) %>% 
  terra::sort("Feature") %>% 
  terra::merge(select(lookup,Type,Feature,Values), all.x=TRUE, by.x=c('Type', 'Feature'), by.y=c('Type', 'Feature'))
toc()

# read in potential multipoints
# merge with values we need to transfer when rasterising
potential_pts = vect(lapply(paste0(scratch_path,potential_pts_files),vect)) %>% 
  terra::sort("Feature") %>% 
  terra::merge(select(lookup,Type,Feature,Values), all.x=TRUE, by.x=c('Type', 'Feature'), by.y=c('Type', 'Feature'))

# terra::rasterize does not currently always correctly transfer names when rasterising
# this is a safeguard against it
correct_ppts_names = sapply(split(potential_pts,"Feature"), function(i) i[["Feature"]][1])

cat("rasterising polygons and points\n")

# rasterise potential multipoints
# transfer values for binary info retention when summing features
tic("rasterise potential points")
rasterize(potential_pts, raster_WGS, field="Values", by="Feature", fun=function(x) min(x,na.rm=TRUE), background=0, filename=paste0(scratch_path,"potential_pts.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S", names=correct_ppts_names))
toc()

# terra::rasterize does not currently always correctly transfer names when rasterising
# this is a safeguard against it
correct_ppolys_names = sapply(split(potential_polys,"Feature"), function(i) i[["Feature"]][1])

# rasterise potential polygons
# transfer values for binary info retention when summing features
tic("rasterise potential polygons")
rasterize(potential_polys, raster_WGS, field="Values", by="Feature", fun="min", touches=TRUE, background=0, filename=paste0(scratch_path,"potential_polys.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S",names=correct_ppolys_names))
toc()

cat("importing already rasterised input data\n")

# some input data are already rasterised
# list files, import and stack
# order layers alphabetically and transfer values for binary info retention when summing features
potential_rfiles = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^P_") & str_detect(.,".tif") & !str_detect(.,"_uncertainty|_old"))
potential_rasters = rast(lapply(paste0(scratch_path,potential_rfiles),rast))
potential_rasters = potential_rasters[[order(names(potential_rasters))]]
tic("reclass input rasters")
potential_rasters = potential_rasters*(lookup[lookup$Feature %in% names(potential_rasters),"Values"])
toc()

# stack rasterised multipoints, polygons and existing rasters
potential = c(rast(paste0(scratch_path,"potential_polys.tif")),
              rast(paste0(scratch_path,"potential_pts.tif")),
              potential_rasters)

# features occassionally count as both likely and potential
# these lines ensure they are handled correctly in both cateories
potential_duplicates = duplicated(names(potential)) | duplicated(names(potential),fromLast=TRUE)

if(any(potential_duplicates)){
  potential_duplicate_stack = potential[[potential_duplicates]]
  
  potential_unique_stack = potential[[!potential_duplicates]]
  
  potential_duplicate_stack = rast(lapply(unique(names(potential_duplicate_stack)),function(x){
    omah = which(x==names(potential_duplicate_stack))
    lay = potential_duplicate_stack[[omah]] |>
      app(max)
    names(lay) = x
    return(lay)
  }))
  
  potential = c(potential_unique_stack,potential_duplicate_stack)
} else{}

# sum raster layers to output single raster with info retained on features
cat("summing potential raster stack\n")
tic("summing potential raster stack")
app(potential,sum,filename=paste0(scratch_path,"potential_sum.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S"))
toc()

# stack likely and potential summed rasters
cat("combining likely and potential rasters and saving\n")
tic("combine likely and potential rasters")
combined = c(rast(paste0(scratch_path,"potential_sum.tif")),rast(paste0(scratch_path,"likely_sum.tif")))

# sum likely and potential rasters to output single raster with info retained on features
app(combined, sum, filename=paste0(scratch_path,"likely_potential_sum.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S"))
toc()

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
  pivot_longer(2:ncol(.), names_to = "Type_Feature", values_to = "Join")

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
  dplyr::select(-ID) %>% 
  mutate(across(CH:last_col(),as.integer))

# ArcGIS needs shortened field names
# match short name from lookup and replace RAT names
shorts = lookup$Short[match(names(final_rat),lookup$Type_Feature)] %>% discard(is.na(.))

names(final_rat) <- c("VALUE","COUNT","CH","C1","C2","C3","C4","C5",shorts)

cat("Reclassifying raster...\n")

final_rat = final_rat %>% dplyr::select(VALUE:C5,sort(shorts))

# reclassify output raster to our more sensible values
rss = classify(rast(paste0(scratch_path,"likely_potential_sum.tif")),as.matrix(cbind(sort(ID$ID),1:nrow(ID))))

# saving files and removing previous versions
cat("Saving raster...\n")

rast_save(rst=rss,filename=paste0("Drill_Down_Critical_Habitat",format(Sys.time(), "_%d%m%Y"),".tif"),outpath=output_path,nms="Drill_Down_Critical_Habitat",dt="INT2U")

cat("Saving RAT...\n")

dbf_file = paste0(output_path,"Drill_Down_Critical_Habitat",format(Sys.time(), "_%d%m%Y"),".tif.vat.dbf")
old_fnm = str_replace(dbf_file,".tif.vat.dbf","_old.tif.vat.dbf")

if(old_fnm %in% list.files(output_path, full.names=TRUE)){
  file.remove(old_fnm)
  } else{}
  
if(dbf_file %in% list.files(output_path, full.names=TRUE)){
  file.rename(dbf_file,old_fnm)
  } else{}
  
foreign::write.dbf(as.data.frame(final_rat), file = dbf_file)

toc()