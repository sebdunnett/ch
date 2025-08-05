##############################################
#### CREATE BASIC CRITICAL HABITAT RASTER ####
##############################################

# Author: Seb Dunnett
# Created: 16/02/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra,tidyverse,foreign)

# import helper functions
source("PATH-TO-0 spatial_processing_functions.R")

# path where drill down is saved
output_path = "OUTPUT-PATH"

# read in raster file name and raster attribute tables
ch_files = list.files(output_path, pattern = "Drill_Down_Critical_Habitat.*\\.tif$", full.names=TRUE)
ch_file = ch_files[which.max(file.info(ch_files)$ctime)]

rat_files = list.files(output_path, pattern = "Drill_Down_Critical_Habitat.*\\.vat\\.dbf$", full.names=TRUE)
rat_file = rat_files[which.max(file.info(rat_files)$ctime)]
rat = foreign::read.dbf(rat_file)

# reclassify raster to CH values
rc = classify(x = rast(ch_file),
                  rcl = as.matrix(dplyr::select(rat,VALUE,CH)))

# save
rast_save(rst=rc,filename=paste0("Basic_Critical_Habitat",format(Sys.time(), "_%d%m%Y"),".tif"),
          outpath=output_path,nms="Basic_Critical_Habitat",dt="INT1U")