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

lookup = read.csv("scripts_tools/lookup.csv") %>% 
  arrange(Type,Feature)

feature_lookup = pull(lookup,Feature,Short)

ch_rast = rast(paste0(output_path,"Critical_Habitat_Drill_Down_WGS.tif"))
activeCat(ch_rast) = "VALUE"
ch_polys = as.polygons(ch_rast) |>
  st_as_sf()
ch_df = foreign::read.dbf(paste0(output_path,"Critical_Habitat_Drill_Down_WGS.tif.vat.dbf"))
ch_polys_full = left_join(ch_polys,ch_df,by="VALUE")

criteria_join = ch_df |>
  dplyr::select(VALUE,C1:C5) |>
  pivot_longer(C1:C5,names_to="Criteria",values_to="Criteria_val") |>
  filter(Criteria_val!=0) |>
  group_by(VALUE) |>
  summarise(CRITERIA=paste(Criteria,collapse="; "))

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

st_save(sf=out,filename="Critical_Habitat_Drill_Down_Polygons.gpkg",outpath=output_path)
