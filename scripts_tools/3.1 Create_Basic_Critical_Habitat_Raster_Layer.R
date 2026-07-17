##############################################
#### CREATE BASIC CRITICAL HABITAT RASTER ####
##############################################

# Author: Seb Dunnett
# Created: 16/02/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra,tidyverse)

# import helper functions
source("scripts_tools/0 spatial_processing_functions.R")

# path where drill down is saved
output_path = "outputs/"

# read in raster file name and raster attribute tables
ch_files = list.files(output_path, pattern = "Drill_Down_Critical_Habitat.*\\.tif$", full.names=TRUE)
ch_file = ch_files[which.max(file.info(ch_files)$ctime)]

# reclassify raster to CH values
rc = rast(ch_file) |>
  as.numeric("CH")

# save
rast_save(rst=rc,filename=paste0("Basic_Critical_Habitat",format(Sys.time(), "_%d%m%Y"),".tif"),
          outpath=output_path,nms="Basic_Critical_Habitat",dt="INT1U")