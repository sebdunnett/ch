##############################################
#### CREATE BASIC CRITICAL HABITAT RASTER ####
##############################################

# Author: Seb Dunnett
# Created: 16/02/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra,tidyverse,foreign)

# import helper functions
source("scripts_tools/0 spatial_processing_functions.R")

# path where drill down is saved
output_path = "outputs/"

# read in raster attribute tables
rat = foreign::read.dbf(paste0(output_path,"Drill_Down_Critical_Habitat.tif.vat.dbf"))

# reclassify raster to CH values
rc = classify(x = rast(paste0(output_path,"Drill_Down_Critical_Habitat.tif")),
                  rcl = as.matrix(rat %>% dplyr::select(VALUE,CH)))

# save
rast_save(rst=rc,filename="Basic_Critical_Habitat.tif",
          outpath=output_path,nms="Basic_Critical_Habitat",dt="INT1U")