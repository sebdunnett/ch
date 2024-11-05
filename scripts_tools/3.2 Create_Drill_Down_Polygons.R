######################################################################
#### CREATE DRILL DOWN POLYGONS ######################################
######################################################################

# Author: Seb Dunnett
# Created: 11/10/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,terra,tidyverse,units,foreign)

output_path = "outputs/"
scratch_path = "scratch/"

# import helper functions
source("scripts_tools/0 spatial_processing_functions.R")

# feature lookups
lookup = read.csv("scripts_tools/lookup.csv") %>% 
  arrange(Type,Feature)

feature_lookup = pull(lookup,Feature,Short)

# import CH raster
# set uniqe overlap ID as active layer, i.e. every unique combination of features globally
ch_files = list.files(output_path, pattern = "Drill_Down_Critical_Habitat.*\\.tif$", full.names=TRUE)
ch_file = ch_files[which.max(file.info(ch_files)$ctime)]
ch_rast = rast(ch_file)
activeCat(ch_rast) = "VALUE"

# polygonise (draw polygons around the cells of each unique value)
ch_polys = as.polygons(ch_rast) |>
  st_as_sf()

# import raster attribute table and join to newly made polygons
rat_files = list.files(output_path, pattern = "Drill_Down_Critical_Habitat.*\\.vat\\.dbf$", full.names=TRUE)
rat_file = rat_files[which.max(file.info(rat_files)$ctime)]
ch_df = foreign::read.dbf(rat_file)
ch_polys_full = left_join(ch_polys,ch_df,by="VALUE")

# pivot longer and summarise by each unique combination
# produces one variable with all triggered criteria separated by semicolon
criteria_join = ch_df |>
  dplyr::select(VALUE,C1:C5) |>
  pivot_longer(C1:C5,names_to="Criteria",values_to="Criteria_val") |>
  filter(Criteria_val!=0) |>
  group_by(VALUE) |>
  summarise(CRITERIA=paste(Criteria,collapse="; "))

# pivot longer and summarise by each unique combination
# produces one variable with all trigger features separated by semicolon
features_join = ch_df |>
  dplyr::select(-COUNT:-C5) |>
  pivot_longer(2:last_col(),names_to="Features",values_to="Features_val") |>
  filter(Features_val!=0) |>
  group_by(VALUE) |>
  summarise(ALL_FEATURES=paste(sort(feature_lookup[Features]),collapse="; "),
            C1=na_if(paste(sort(keep(feature_lookup[Features], feature_lookup[Features] %in% lookup[lookup$C1==1,]$Feature)),collapse="; "),""),
            C2=na_if(paste(sort(keep(feature_lookup[Features], feature_lookup[Features] %in% lookup[lookup$C2==1,]$Feature)),collapse="; "),""),
            C3=na_if(paste(sort(keep(feature_lookup[Features], feature_lookup[Features] %in% lookup[lookup$C3==1,]$Feature)),collapse="; "),""),
            C4=na_if(paste(sort(keep(feature_lookup[Features], feature_lookup[Features] %in% lookup[lookup$C4==1,]$Feature)),collapse="; "),""),
            C5=na_if(paste(sort(keep(feature_lookup[Features], feature_lookup[Features] %in% lookup[lookup$C5==1,]$Feature)),collapse="; "),""))

# reclassify CH codes to something more meaningful
# for ease, remove all "Unclassified" polygons to leave only polgons where CH is triggered
out = reduce(list(ch_polys,dplyr::select(ch_df,VALUE,CH),criteria_join,features_join),left_join,by="VALUE") |>
  mutate(CH = case_match(
    CH,
    0 ~ "Unclassified",
    1 ~ "Potential",
    10 ~ "Likely"
    )) |>
  filter(CH!="Unclassified") |>
  dplyr::select(-VALUE) |>
  arrange(CH,CRITERIA,ALL_FEATURES)

# save
st_save(sf=out,filename=paste0("Drill_Down_Critical_Habitat_Polygons",format(Sys.time(), "_%d%m%Y"),".gpkg"),outpath=output_path)
