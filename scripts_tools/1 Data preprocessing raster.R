################################################################
#### PREPROCESSING NON-RED LIST DATA (RASTERS) #################
################################################################

# Author: Seb Dunnett
# Created: 24/07/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,terra,sf)

cat("read in template rasters\n")

data_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/"

output_path <- "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

raster_WGS <- rast('O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif')

raster_moll <- rast(crs="ESRI:54009",
                      res=1000,
                      ext=ext(c(-18040095.7,18040095.7,-9020047.85,9020047.85)))

################################################################
#### MODELLED COLDWATER CORAL ##################################
################################################################

cat("coldwater coral...")

coldwater_coral_modelled = rast(paste0(data_path,"Yesson_2012_ColdWaterCorals_Modelled/Yesson_2012_ColdWaterCorals_Modelled.tif"))

crs(coldwater_coral_modelled) = "ESRI:54034" # Cylindrical Equal Area proj

P_C4_C5_Coldwater_coral_WGS = project(coldwater_coral_modelled, raster_WGS, method="near") %>% 
  classify(cbind(NA,0))
P_C4_C5_Coldwater_coral_moll = project(coldwater_coral_modelled, raster_moll, method="near") %>% 
  classify(cbind(NA,0))

cat("done\n")

################################################################
#### EVERWET FORESTS ###########################################
################################################################

cat("everwet forests...")

everwet = rast(paste0(data_path,"Everwet_Zones/Everwet_Zones/everwet_zones")) %>%
  classify(rbind(c(0,NA),c(1,1),c(2,1),c(3,1)))

crs(everwet) = "ESRI:54034" # Cylindrical Equal Area proj

P_C4_Everwet_Zones_WGS = project(everwet, raster_WGS, method="near") %>% 
  classify(cbind(NA,0))
P_C4_Everwet_Zones_moll = project(everwet, raster_moll, method="near") %>% 
  classify(cbind(NA,0))

cat("done\n")

################################################################
#### TROPICAL MOIST FOREST #####################################
################################################################

cat("tropical moist forests...")

L_C4_Tropical_Moist_Forest_WGS = rast(paste0(data_path,"Tropical Moist Forest/tmf_mosaic.tif")) %>% 
  resample(raster_WGS, method="near") %>% 
  classify(cbind(NA,0))

L_C4_Tropical_Moist_Forest_moll = project(rast(paste0(data_path,"Tropical Moist Forest/tmf_mosaic.tif")), raster_moll, method="near") %>% 
  classify(cbind(NA,0))

cat("done\n")

################################################################
#### TROPICAL DRY FOREST #######################################
################################################################

cat("tropical dry forests...")

tropical_dry_forest_500m = rast(paste0(data_path,"WCMC_065_TropicalDryForests2006/Tropical_Dry_Forests/trop_dryf/tropdryf"))

P_C4_Tropical_Dry_Forest_WGS = project(tropical_dry_forest_500m, raster_WGS, method="near") %>% 
  classify(cbind(NA,0))
P_C4_Tropical_Dry_Forest_moll = project(tropical_dry_forest_500m, raster_moll, method="near") %>%
  classify(cbind(NA,0))

cat("done\n")

################################################################
#### WRITE RASTERS #############################################
################################################################

cat("writing rasters...")

# check output folder for previously made files
# rename old files (and delete older)
# save
coldwater_coral_WGS_file = paste0(output_path,"P_C4_C5_Coldwater_coral_WGS.tif")
coldwater_coral_moll_file = paste0(output_path,"P_C4_C5_Coldwater_coral_moll.tif")

everwet_WGS_file = paste0(output_path,"P_C4_Everwet_Zones_WGS.tif")
everwet_moll_file = paste0(output_path,"P_C4_Everwet_Zones_moll.tif")

tropical_moist_WGS_file = paste0(output_path,"L_C4_Tropical_Moist_Forest_WGS.tif")
tropical_moist_moll_file = paste0(output_path,"L_C4_Tropical_Moist_Forest_moll.tif")

tropical_dry_WGS_file = paste0(output_path,"P_C4_Tropical_Dry_Forest_WGS.tif")
tropical_dry_moll_file = paste0(output_path,"P_C4_Tropical_Dry_Forest_moll.tif")

output_files = c(coldwater_coral_WGS_file, coldwater_coral_moll_file,
                 everwet_WGS_file, everwet_moll_file,
                 tropical_moist_WGS_file, tropical_moist_moll_file,
                 tropical_dry_WGS_file, tropical_dry_moll_file)

if((output_files %in% list.files(output_path, full.names = TRUE) %>% sum)>1){
  file.remove(str_replace(output_files,".tif","_old.tif"))
  file.rename(output_files,str_replace(output_files,".tif","_old.tif"))
} else{}

writeRaster(P_C4_C5_Coldwater_coral_WGS, coldwater_coral_WGS_file, datatype = "FLT8S")
writeRaster(P_C4_C5_Coldwater_coral_moll, coldwater_coral_moll_file, datatype = "FLT8S")

writeRaster(P_C4_Everwet_Zones_WGS, everwet_WGS_file, datatype = "FLT8S")
writeRaster(P_C4_Everwet_Zones_moll, everwet_moll_file, datatype = "FLT8S")

writeRaster(L_C4_Tropical_Moist_Forest_WGS, tropical_moist_WGS_file, datatype = "FLT8S")
writeRaster(L_C4_Tropical_Moist_Forest_moll, tropical_moist_moll_file, datatype = "FLT8S")

writeRaster(P_C4_Tropical_Dry_Forest_WGS, tropical_dry_WGS_file, datatype = "FLT8S")
writeRaster(P_C4_Tropical_Dry_Forest_moll, tropical_dry_moll_file, datatype = "FLT8S")

cat("done\n")