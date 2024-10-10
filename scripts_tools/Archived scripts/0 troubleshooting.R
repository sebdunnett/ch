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
terraOptions(datatype="FLT8S", memfrac=0.9)

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

lookup$Values = values

potential_pts_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^P_") & str_detect(.,"_pts.shp"))

potential_pts = vect(lapply(paste0(scratch_path,potential_pts_files),vect)) %>% 
  terra::sort("Feature") %>% 
  terra::merge(select(lookup,Type,Feature,Values), all.x=TRUE, by.x=c('Type', 'Feature'), by.y=c('Type', 'Feature'))

correct_ppts_names = sapply(split(potential_pts,"Feature"), function(i) i[["Feature"]][1])

tic("rasterize potential pts")
rasterize(potential_pts, raster_WGS, field="Values", by="Feature", fun=function(x) min(x,na.rm=TRUE), background=0, filename=paste0(scratch_path,"potential_WGS_pts2.tif"), overwrite=TRUE, wopt=list(datatype="FLT8S", names=correct_ppts_names))
toc()