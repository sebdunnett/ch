################################################################
#### PREPROCESSING NON-RED LIST DATA (RASTERS) #################
################################################################

# Author: Seb Dunnett
# Created: 24/07/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,terra,sf)

terraOptions(memfrac=0.9)

cat("read in template rasters\n")

data_path <- "raw_data/"

output_path <- "scratch/"

mangrove_path <- "raw_data/GMW_v3/gmw_v3_2020/"

tmf_path <- "raw_data/Tropical Moist Forest/"

raster_WGS <- rast(res=1/120)

# import helper functions
source("scripts_tools/0 spatial_processing_functions.R")

presence_threshold = 0.5

################################################################
#### MODELLED COLDWATER CORAL ##################################
################################################################

cat("coldwater coral...")

soft_files = list.files(paste0(data_path,"ZSL-001-ModelledOctocorals2012/02_Data_sources/Restricted-GeoTiffHighRes"),full.names=TRUE) %>% 
  keep(str_ends(.,".tif")) %>% 
  discard(str_detect(.,"Consensus"))

soft = app(rast(soft_files)>90,any) |>
  resample(raster_WGS,method="near") |>
  classify(cbind(NA,0))

stony_files = list.files(paste0(data_path,"Bangor-001-StonyCorals2011/DataPack-Bangor-001-StonyCorals2011/IndividualSpecies"),full.names=TRUE) |>
  list.files(full.names=TRUE) %>% 
  keep(str_ends(.,".tif"))

stony = app(rast(stony_files)>0.9,any) |>
  resample(raster_WGS,method="near") |>
  classify(cbind(NA,0))

P_C4_C5_Coldwater_coral = app(c(soft,stony),any)

cat("writing...")

rast_save(rst=P_C4_C5_Coldwater_coral,filename="P_C4_C5_Coldwater_coral.tif",outpath=output_path,nms="Cold water Coral - Modelled occurence",dt="FLT8S")

cat("done\n")

################################################################
#### CLOUD FORESTS #############################################
################################################################

cat("cloud forests...")

tcf = rast(paste0(data_path,"tcf_ensemble_mn_sd_2001-2018_v16/tcf/tcf_ensemble_mn_2018_v16.tif"))
L_C4_Cloud_Forest = !is.na(tcf) |>
  project(raster_WGS,method="near")

cat("writing...")

rast_save(rst=L_C4_Cloud_Forest,filename="L_C4_Cloud_Forest.tif",outpath=output_path,nms="Tropical montane cloud forests",dt="FLT8S")

cat("done\n")

################################################################
#### EVERWET FORESTS ###########################################
################################################################

# cat("everwet forests...")
# 
# everwet = rast(paste0(data_path,"Everwet_Zones/Everwet_Zones/everwet_zones")) |>
#   classify(rbind(c(2,0),c(3,1))) |>
#   disagg(fact=5,method="near")
# 
# crs(everwet) = "ESRI:53034" # Cylindrical Equal Area proj
# 
# P_C4_Everwet_Zones = project(everwet, raster_WGS, method="near", threads=TRUE) |>
#   classify(cbind(NA,0))
# 
# cat("writing...")
# 
# rast_save(rst=P_C4_Everwet_Zones,filename="P_C4_Everwet_Zones.tif",outpath=output_path,nms="Ever-wet tropical forests",dt="FLT8S")
# 
# cat("done\n")

################################################################
#### TROPICAL MOIST FOREST #####################################
################################################################

# cat("tropical moist forests...")
# 
# tmf_files = list.files(tmf_path,full.names=TRUE) %>%
#   discard(str_detect(.,"mosaic")) %>%
#   keep(str_ends(.,".tif"))
# 
# cat("remove unnecessary tiles...")
# 
# # some tiles don't have TMF category
# has_cat10 = unlist(lapply(1:length(tmf_files),function(x){
#   cat(paste0(x,"..."))
#   10 %in% terra::unique(rast(tmf_files[x]))[,1]
#   }))
# 
# tmf_files = tmf_files %>% keep(.,has_cat10)
# 
# cat("vrt...")
# 
# vrt(x=tmf_files, filename=paste0(tmf_path,"tmf_mosaic.tif"), overwrite=TRUE)
# 
# cat("aggregate...")
# 
# terra::aggregate(rast(paste0(tmf_path,"tmf_mosaic.tif")),
#                  fact=round(res(raster_WGS)/res(rast(tmf_files[1]))),
#                  fun=function(x,...){sum((x == 10),na.rm = T)/length(x)},
#                  filename=paste0(tmf_path,"tmf_mosaic_agg.tif"),
#                  overwrite=TRUE)
# 
# cat("resample and project...")
# 
# L_C4_Tropical_Moist_Forest = rast(paste0(tmf_path,"tmf_mosaic_agg.tif")) %>%
#   resample(raster_WGS, method="bilinear") %>%
#   classify(cbind(NA,0)) %>%
#   classify(rbind(c(0,presence_threshold,0),c(presence_threshold,1,1)))
# 
# cat("writing...")
# 
# rast_save(rst=L_C4_Tropical_Moist_Forest,filename="L_C4_Tropical_Moist_Forest.tif",outpath=output_path,nms="Tropical moist forest",dt="FLT8S")
# 
# cat("done\n")

################################################################
#### TROPICAL DRY FOREST #######################################
################################################################

# cat("tropical dry forests...")
# 
# tropical_dry_forest = rast(paste0(data_path,"WCMC_065_TropicalDryForests2006/Tropical_Dry_Forests/trop_dryf/tropdryf")) |>
#   classify(cbind(NA,0)) |>
#   aggregate(fact=2,fun="modal",na.rm=TRUE) |>
#   extend(raster_WGS,fill=0)
# 
# P_C4_Tropical_Dry_Forest = resample(tropical_dry_forest,raster_WGS,method="near")
# 
# cat("writing...")
# 
# rast_save(rst=P_C4_Tropical_Dry_Forest,filename="P_C4_Tropical_Dry_Forest.tif",outpath=output_path,nms="Tropical dry forest",dt="FLT8S")
# 
# cat("done\n")

################################################################
#### MANGROVES (POLYGON) #######################################
################################################################

#cat("mangroves...")
#
# mangrove_files = list.files(mangrove_path, full.names = TRUE) %>%
#   discard(str_detect(.,"mosaic")) %>% 
#   keep(str_ends(.,".tif"))
# 
# cat("vrt...")
# 
# vrt(x=mangrove_files, filename=paste0(mangrove_path,"mangrove_mosaic.tif"), overwrite=TRUE)
# 
# cat("aggregate...")
# 
# terra::aggregate(rast(paste0(mangrove_path,"mangrove_mosaic.tif")),
#                  fact=round(res(raster_WGS)/res(rast(mangrove_files[1]))),
#                  fun=function(x,...){sum(x,na.rm=T)/length(x)},
#                  filename=paste0(mangrove_path,"mangrove_mosaic_agg.tif"),
#                  overwrite=TRUE)
# 
# cat("resample and reproject...")
# 
# L_C4_Mangrove = rast(paste0(mangrove_path,"mangrove_mosaic_agg.tif")) %>%
#   resample(raster_WGS, method="bilinear") %>%
#   classify(cbind(NA,0)) %>%
#   classify(rbind(c(0,presence_threshold,0),c(presence_threshold,1,1)))
# 
# cat("writing...")
# 
# rast_save(rst=L_C4_Mangrove,filename="L_C4_Mangrove.tif",outpath=output_path,nms="Mangroves",dt="FLT8S")
# 
# cat("done\n")

################################################################
#### Great Apes ################################################
################################################################

cat("Great Apes...")

ga_files = list.files(path=paste0(data_path,"Mammals_primates"),pattern=c("Pongo|Pan|Gorilla"),full.names=TRUE)

ga = lapply(1:length(ga_files),function(x){
  rst = rast(ga_files[x]) |>
    aggregate(fact=10,fun="modal",na.rm=TRUE) |>
    extend(raster_WGS) |>
    resample(raster_WGS,method="near") |>
    classify(cbind(NA,0))
  return(rst)
})

L_C1_Great_Apes_AoH = app(rast(ga),any)

cat("writing...")

rast_save(rst=L_C1_IUCN_Great_Apes,filename="L_C1_Great_Apes_AoH.tif",outpath=output_path,nms="Great Apes habitat",dt="FLT8S")

cat("done\n")