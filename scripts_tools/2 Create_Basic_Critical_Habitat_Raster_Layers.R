################################################################
####        CREATE BASIC CRITICAL HABITAT RASTER            ####
################################################################

# Author: Seb Dunnett
# Created: 16/02/2023

# This script can be run once the polygon files have been exported from Google Earth Engine
# These scripts can be found:
# Potential: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FPotential_Critical_Habitat
# Likely: https://code.earthengine.google.com/?scriptPath=users%2Fcorinnaravilious%2FUNEP-WCMC_SharedScripts%3Ap08868_Critical_Habitat_Update%2FLikely_Critical_Habitat

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster,tidyverse,tictoc,foreign)

# path where drill down is saved
output_path = "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/"

# read in raster attribute tables
rat_WGS = foreign::read.dbf(paste0(output_path,"Critical_Habitat_Drill_Down_WGS.tif.vat.dbf"))
rat_moll = foreign::read.dbf(paste0(output_path,"Critical_Habitat_Drill_Down_Mollweide.tif.vat.dbf"))

# set filenames
WGS_file = paste0(output_path,"Basic_Critical_Habitat_Raster_WGS.tif")
moll_file = paste0(output_path,"Basic_Critical_Habitat_Raster_Mollweide.tif")

# reclassify rasters from unique IDs to likely (10), potential (0) and unknown (0)
if(file.exists(WGS_file)){
  cat("Remove or archive previous WGS basic critical habitat raster\n")
} else{
  cat("Saving WGS raster\n")
  reclassify(x = raster(paste0(output_path,"Critical_Habitat_Drill_Down_WGS.tif")),
           rcl = as.matrix(rat_WGS %>% dplyr::select(VALUE,CH)),
           filename = WGS_file)
}

if(file.exists(moll_file)){
  cat("Remove or archive previous Mollweide basic critical habitat raster\n")
} else{
  cat("Saving Mollweide raster\n")
  reclassify(x = raster(paste0(output_path,"Critical_Habitat_Drill_Down_Mollweide.tif")),
             rcl = as.matrix(rat_moll %>% dplyr::select(VALUE,CH)),
             filename = moll_file)
}