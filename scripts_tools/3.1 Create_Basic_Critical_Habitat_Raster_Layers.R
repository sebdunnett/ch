################################################################
####        CREATE BASIC CRITICAL HABITAT RASTER            ####
################################################################

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
rat_WGS = foreign::read.dbf(paste0(output_path,"Critical_Habitat_Drill_Down_WGS.tif.vat.dbf"))
rat_moll = foreign::read.dbf(paste0(output_path,"Critical_Habitat_Drill_Down_Moll.tif.vat.dbf"))

rc_WGS = classify(x = rast(paste0(output_path,"Critical_Habitat_Drill_Down_WGS.tif")),
                  rcl = as.matrix(rat_WGS %>% dplyr::select(VALUE,CH)))
rc_moll = classify(x = rast(paste0(output_path,"Critical_Habitat_Drill_Down_Moll.tif")),
                  rcl = as.matrix(rat_moll %>% dplyr::select(VALUE,CH)))

rast_save(rst=rc_WGS,filename="Basic_Critical_Habitat_Raster_WGS.tif",
          outpath=output_path,nms="Basic_Critical_Habitat_Raster_WGS",dt="INT1U")
rast_save(rst=rc_moll,filename="Basic_Critical_Habitat_Raster_Moll.tif",
          outpath=output_path,nms="Basic_Critical_Habitat_Raster_Moll",dt="INT1U")