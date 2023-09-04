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

sphere <- st_graticule(ndiscr = 10000, margin = 10e-6) %>%
  st_transform(crs = st_crs("ESRI:54009")) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))

raster_moll_mask = rasterize(sphere,raster_moll)

rast_save <- function(rst,filename,outpath){
  fnm = paste0(outpath,filename)
  old_fnm = str_replace(fnm,".tif","_old.tif")
  
  if(old_fnm %in% list.files(outpath, full.names=TRUE)){
    file.remove(old_fnm)
  } else{}
  
  if(fnm %in% list.files(outpath, full.names=TRUE)){
    file.rename(fnm,old_fnm)
  } else{}
  
  writeRaster(rst,fnm,datatype = "FLT8S")
}

################################################################
#### MODELLED COLDWATER CORAL ##################################
################################################################

# cat("coldwater coral...")
# 
# coldwater_coral_modelled = rast(paste0(data_path,"Yesson_2012_ColdWaterCorals_Modelled/Yesson_2012_ColdWaterCorals_Modelled.tif"))
# 
# crs(coldwater_coral_modelled) = "ESRI:54034" # Cylindrical Equal Area proj
# 
# P_C4_C5_Coldwater_coral_WGS = project(coldwater_coral_modelled, raster_WGS, method="near", threads=TRUE) %>% 
#   classify(cbind(NA,0))
# P_C4_C5_Coldwater_coral_moll = project(coldwater_coral_modelled, raster_moll_mask, method="near", mask=TRUE, threads=TRUE)
# 
# cat("done\n")

################################################################
#### EVERWET FORESTS ###########################################
################################################################

# cat("everwet forests...")
# 
# everwet = rast(paste0(data_path,"Everwet_Zones/Everwet_Zones/everwet_zones")) %>%
#   classify(rbind(c(0,NA),c(1,1),c(2,1),c(3,1)))
# 
# crs(everwet) = "ESRI:54034" # Cylindrical Equal Area proj
# 
# P_C4_Everwet_Zones_WGS = project(everwet, raster_WGS, method="near", threads=TRUE) %>% 
#   classify(cbind(NA,0))
# P_C4_Everwet_Zones_moll = project(everwet, raster_moll_mask, method="near", mask=TRUE, threads=TRUE)
# 
# cat("done\n")

################################################################
#### TROPICAL MOIST FOREST #####################################
################################################################

cat("tropical moist forests...")

tmf_files = list.files(paste0(data_path,"Tropical Moist Forest"),full.names=TRUE) %>% discard(str_detect(.,"mosaic"))

cat("remove unnecessary tiles...")

# some tiles don't have TMF category
has_cat10 = unlist(lapply(1:length(tmf_files),function(x){
  cat(paste0(x,"..."))
  10 %in% terra::unique(rast(tmf_files[x]))[,1]
  }))

tmf_files = tmf_files %>% keep(.,has_cat10)

cat("vrt...")

vrt(x=tmf_files, filename=paste0(data_path,"Tropical Moist Forest/tmf_mosaic.tif"), overwrite=TRUE)

cat("aggregate...")

terra::aggregate(rast(paste0(data_path,"Tropical Moist Forest/tmf_mosaic.tif")),
                 fact=round(res(raster_WGS)/res(rast(tmf_files[1]))),
                 fun=function(x,...){sum((x == 10),na.rm = T)/length(x)},
                 filename=paste0(data_path,"Tropical Moist Forest/tmf_mosaic_agg.tif"),
                 overwrite=TRUE)

cat("resample and project...")

L_C4_Tropical_Moist_Forest_WGS = rast(paste0(data_path,"Tropical Moist Forest/tmf_mosaic_agg.tif")) %>% 
  resample(raster_WGS, method="bilinear") %>% 
  classify(cbind(NA,0))

L_C4_Tropical_Moist_Forest_moll = project(L_C4_Tropical_Moist_Forest_WGS, raster_moll_mask, method="near", mask=TRUE, threads=TRUE)

cat("done\n")

################################################################
#### TROPICAL DRY FOREST #######################################
################################################################

# cat("tropical dry forests...")
# 
# tropical_dry_forest_500m = rast(paste0(data_path,"WCMC_065_TropicalDryForests2006/Tropical_Dry_Forests/trop_dryf/tropdryf"))
# 
# P_C4_Tropical_Dry_Forest_WGS = project(tropical_dry_forest_500m, raster_WGS, method="near", threads=TRUE) %>% 
#   classify(cbind(NA,0))
# P_C4_Tropical_Dry_Forest_moll = project(tropical_dry_forest_500m, raster_moll_mask, method="near", mask=TRUE, threads=TRUE)
# 
# cat("done\n")

################################################################
#### MANGROVES (POLYGON) #######################################
################################################################

cat("mangroves...")

mangrove_files = list.files(paste0(data_path,"GMW_v3/gmw_v3_2020"), full.names = TRUE) %>%
  discard(str_detect(.,"mosaic"))

cat("vrt...")

vrt(x=mangrove_files, filename=paste0(data_path,"GMW_v3/gmw_v3_2020/mosaic.tif"), overwrite=TRUE)

cat("aggregate...")

terra::aggregate(rast(paste0(data_path,"GMW_v3/gmw_v3_2020/mosaic.tif")),
                 fact=round(res(raster_WGS)/res(rast(mangrove_files[1]))),
                 fun=function(x,...){sum(x,na.rm=T)/length(x)},
                 filename=paste0(data_path,"GMW_v3/gmw_v3_2020/mosaic_agg.tif"),
                 overwrite=TRUE)

cat("resample and reproject...")

L_C4_Mangrove_WGS = rast(paste0(data_path,"GMW_v3/gmw_v3_2020/mosaic_agg.tif")) %>% 
  resample(raster_WGS, method="bilinear") %>% 
  classify(cbind(NA,0))

L_C4_Mangrove_moll = project(L_C4_Mangrove_WGS, raster_moll_mask, method="bilinear", mask=TRUE, threads=TRUE)

cat("done\n")

################################################################
#### WRITE RASTERS #############################################
################################################################

cat("writing rasters...")

#rast_save(rst=P_C4_C5_Coldwater_coral_WGS,filename="P_C4_C5_Coldwater_coral_WGS.tif",outpath=output_path)
#rast_save(rst=P_C4_C5_Coldwater_coral_moll,filename="P_C4_C5_Coldwater_coral_moll.tif",outpath=output_path)

#rast_save(rst=P_C4_Everwet_Zones_WGS,filename="P_C4_Everwet_Zones_WGS.tif",outpath=output_path)
#rast_save(rst=P_C4_Everwet_Zones_moll,filename="P_C4_Everwet_Zones_moll.tif",outpath=output_path)

rast_save(rst=L_C4_Tropical_Moist_Forest_WGS,filename="L_C4_Tropical_Moist_Forest_WGS.tif",outpath=output_path)
rast_save(rst=L_C4_Tropical_Moist_Forest_moll,filename="L_C4_Tropical_Moist_Forest_moll.tif",outpath=output_path)

#rast_save(rst=P_C4_Tropical_Dry_Forest_WGS,filename="P_C4_Tropical_Dry_Forest_WGS.tif",outpath=output_path)
#rast_save(rst=P_C4_Tropical_Dry_Forest_moll,filename="P_C4_Tropical_Dry_Forest_moll.tif",outpath=output_path)

rast_save(rst=L_C4_Mangrove_WGS,filename="L_C4_Mangrove_WGS.tif",outpath=output_path)
rast_save(rst=L_C4_Mangrove_moll,filename="L_C4_Mangrove_moll.tif",outpath=output_path)

cat("done\n")