################################################################
#### PREPROCESSING NON-RED LIST DATA (VECTORS) #################
################################################################

# Author: Seb Dunnett
# Created: 20/07/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,tidyverse,terra,tictoc,geos)

data_path = "raw_data/"

output_path = "scratch/"

# import helper functions
source("scripts_tools/spatial_processing_functions.R")

tic("read in WDPA and KBAs")
cat("read in WDPA and KBAs...")

wdpa_gdb = "O:/f00_data/Protected_Planet_Initiative/WDPA_Monthly_Versions/Latest WDPA Version/WDPA_Nov2023_Licensed.gdb"

wdpa_poly_layer = st_layers(wdpa_gdb)$name %>% keep(.,str_detect(.,"poly"))
wdpa_pt_layer = st_layers(wdpa_gdb)$name %>% keep(.,str_detect(.,"point"))

wdpa_polys = st_read(wdpa_gdb, layer=wdpa_poly_layer, quiet=TRUE)
wdpa_pts = st_read(wdpa_gdb, layer=wdpa_pt_layer, quiet=TRUE)

kba_aze_iba_polys = st_read(dsn = "O:/f00_data/Birdlife-001-KBA-IBA-AZE/KBAsGlobal_2023_September_02_Criteria_TriggerSpecies/KBAsGlobal_2023_September_02_POL.shp", quiet=TRUE)
kba_aze_iba_pts = st_read(dsn = "O:/f00_data/Birdlife-001-KBA-IBA-AZE/KBAsGlobal_2023_September_02_Criteria_TriggerSpecies/KBAsGlobal_2023_September_02_PNT.shp", quiet=TRUE)

cat("done\n")
toc()

`%ni%` = Negate(`%in%`)

# ################################################################
# #### SWOT TURTLE NESTS (POINT) #################################
# ################################################################
# 
# cat("sea turtle nesting...")
# 
# swot = st_read(paste0(data_path,"obis_seamap_swot/obis_seamap_swot_5f7dd60721f10_20201007_105234_site_locations_shapefile.shp"), quiet=TRUE)
# 
# L_C1_Turtle_CREN = filter(swot,commonname %in% c("Green Sea Turtle","Hawksbill Sea Turtle","Kemp's Ridley")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Sea turtle nesting sites - CR and EN species")
# 
# P_C3_C4_Turtle_all = swot %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "Sea turtle nesting sites - All species")
# 
# cat("saving...")
# 
# st_save(sf=L_C1_Turtle_CREN, filename="L_C1_Turtle_CREN_pts.shp", outpath=output_path)
# st_save(sf=P_C3_C4_Turtle_all, filename="P_C3_C4_Turtle_all_pts.shp", outpath=output_path)
# 
# cat("done\n")
# 
# ################################################################
# #### CLOUD FORESTS (POINT) #####################################
# ################################################################
# 
# cat("cloud forests...")
# 
# P_C4_Cloud_Forest = st_read(paste0(data_path,"cloud_forests/cloud_forest_points_1997.shp"), quiet=TRUE) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "Tropical montane cloud forests")
# 
# cat("saving...")
# 
# st_save(sf=P_C4_Cloud_Forest, filename="P_C4_Cloud_Forest_pts.shp", outpath=output_path)
# 
# cat("done\n")
# 
# ################################################################
# #### COLD SEEPS (POINT) ########################################
# ################################################################
# 
# cat("cold seeps...")
# 
# cold_seeps = st_read(paste0(data_path,"ChEssBase_20200910_v1_1/ChEssBase_20200910_v1_1_occurrence.shp"), quiet=TRUE)
# 
# L_C4_C5_Cold_seeps = cold_seeps %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Cold seeps")
# 
# P_C2_Cold_seeps = cold_seeps %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "Cold seeps")
# 
# cat("saving...")
# 
# st_save(sf=L_C4_C5_Cold_seeps, filename="L_C4_C5_Cold_seeps_pts.shp", outpath=output_path)
# st_save(sf=P_C2_Cold_seeps, filename="P_C2_Cold_seeps_pts.shp", outpath=output_path)
# 
# cat("done\n")
# 
# ################################################################
# #### OBSERVED COLDWATER CORAL (POINT & POLY) ###################
# ################################################################
# 
# cat("observed coldwater coral...")
# 
# L_C4_C5_Cold_water_coral_observed_pts = st_read(paste0(data_path,"14_001_WCMC001_ColdCorals2017_v5_1/01_Data/WCMC001_ColdCorals2017_Pt_v5_1.shp"), quiet=TRUE) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Cold water coral reefs - Observed occurence")
# 
# tic("coldwater coral polys read in, fix and union")
# L_C4_C5_Cold_water_coral_observed_polys = st_read(paste0(data_path,"14_001_WCMC001_ColdCorals2017_v5_1/01_Data/WCMC001_ColdCorals2017_Py_v5_1.shp"), quiet=TRUE) %>%
#   fix_sf() %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Cold water coral reefs - Observed occurence")
# toc()
# 
# cat("saving...")
# 
# st_save(sf=L_C4_C5_Cold_water_coral_observed_pts, filename="L_C4_C5_Cold_water_coral_observed_pts.shp", outpath=output_path)
# st_save(sf=L_C4_C5_Cold_water_coral_observed_polys, filename="L_C4_C5_Cold_water_coral_observed_polys.shp", outpath=output_path)
# 
# cat("done\n")
# 
# ################################################################
# #### CORAL REEFS (POINT & POLY) ################################
# ################################################################
# 
# cat("coral reefs...")
# 
# L_C4_C5_Warm_water_coral_pts = st_read(paste0(data_path,"14_001_WCMC008_CoralReefs2018_v4_1/01_Data/WCMC008_CoralReef2018_Pt_v4_1.shp"), quiet=TRUE) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Warm water coral reefs")
# 
# tic("coral reef polys read in, fix and union")
# L_C4_C5_Warm_water_coral_polys = st_read(paste0(data_path,"14_001_WCMC008_CoralReefs2018_v4_1/01_Data/WCMC008_CoralReef2018_Py_v4_1.shp"), quiet=TRUE) %>%
#   fix_sf() %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Warm water coral reefs")
# toc()
# 
# cat("saving...")
# 
# st_save(sf=L_C4_C5_Warm_water_coral_pts, filename="L_C4_C5_Warm_water_coral_pts.shp", outpath=output_path)
# st_save(sf=L_C4_C5_Warm_water_coral_polys, filename="L_C4_C5_Warm_water_coral_polys.shp", outpath=output_path)
# 
# cat("done\n")
# 
# ################################################################
# #### HYDROTHERMAL VENTS (POINT) ################################
# ################################################################
# 
# cat("hydrothermal vents...")
# 
# hydrothermal_vents = st_read(paste0(data_path,"PANGEA_2020_InterRidge_Database_Hydrothermal_Vent_v3_4/vent_fields_all_20200325.shp"), quiet=TRUE)
# 
# L_C2_C5_Hydrothermal_Vents = hydrothermal_vents %>%
#   filter(Activity %in% c("active, inferred","active, confirmed")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Hydrothermal Vents")
# 
# P_C4_Hydrothermal_Vents = hydrothermal_vents %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "Hydrothermal Vents")
# 
# cat("saving...")
# 
# st_save(sf=L_C2_C5_Hydrothermal_Vents, filename="L_C2_C5_Hydrothermal_Vents_pts.shp", outpath=output_path)
# st_save(sf=P_C4_Hydrothermal_Vents, filename="P_C4_Hydrothermal_Vents_pts.shp", outpath=output_path)
# 
# cat("done\n")
# 
# ################################################################
# #### IMMAs (POLYGON) ###########################################
# ################################################################
# 
# cat("immas...")
# 
# tic("immas read in, fix and union")
# imma = st_read(paste0(data_path,"iucn-imma/iucn-imma_oct20.shp"), quiet=TRUE) %>%
#   fix_sf()
# 
# P_C1_IMMAs_A = filter(imma, str_detect(Criteria.2,"A")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "IMMAs under criterion A")
# 
# P_C2_IMMAs_B1 = filter(imma, str_detect(Criteria.2,"B1")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "IMMAs under criterion B1")
# 
# L_C3_IMMAs_B2 = filter(imma, str_detect(Criteria.2,"B2")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "IMMAs under criterion B2")
# 
# P_C3_IMMAs_C1_C2_C3 = filter(imma, str_detect(Criteria.2,"C1|C2|C3")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "IMMAs under criteria C1, C2 and C3")
# 
# P_C2_C4_IMMAs_D1 = filter(imma, str_detect(Criteria.2,"D1")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "IMMAs under criterion D1")
# 
# P_C4_IMMAs_D2 = filter(imma, str_detect(Criteria.2,"D2")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "IMMAs under criterion D2")
# toc()
# 
# cat("saving...")
# 
# st_save(sf=P_C1_IMMAs_A, filename="P_C1_IMMAs_A_polys.shp", outpath=output_path)
# st_save(sf=P_C2_IMMAs_B1, filename="P_C2_IMMAs_B1_polys.shp", outpath=output_path)
# st_save(sf=L_C3_IMMAs_B2, filename="L_C3_IMMAs_B2_polys.shp", outpath=output_path)
# st_save(sf=P_C3_IMMAs_C1_C2_C3, filename="P_C3_IMMAs_C1_C2_C3_polys.shp", outpath=output_path)
# st_save(sf=P_C2_C4_IMMAs_D1, filename="P_C2_C4_IMMAs_D1_polys.shp", outpath=output_path)
# st_save(sf=P_C4_IMMAs_D2, filename="P_C4_IMMAs_D2_polys.shp", outpath=output_path)
# 
# cat("done\n")
# 
# ################################################################
# #### INTACT FOREST LANDSCAPES (POLYGON) ########################
# ################################################################
# 
# cat("intact forest landscapes...")
# 
# tic("intact forest landscapes read in, fix and union")
# L_C4_Intact_Forest_Landscapes = st_read(paste0(data_path,"IFL_2016/ifl_2016.shp"), quiet=TRUE) %>%
#   fix_sf() %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Intact Forest Landscapes")
# toc()
# 
# cat("saving...")
# 
# st_save(sf=L_C4_Intact_Forest_Landscapes, filename="L_C4_Intact_Forest_Landscapes_polys.shp", outpath=output_path)
# 
# cat("done\n")
# 
# ################################################################
# #### IRREPLACEABLE PAs (POLYGON) ###############################
# ################################################################

# cat("irreplaceable PAs...")
# 
# tic("irreplaceable PAs read in, fix and union")
# L_C4_Irrep_PAs = st_read(paste0(data_path,"Irreplaceable/irrep_pa_WDPA_poly_Nov2022.shp"), quiet=TRUE) %>%
#   fix_sf() %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Irreplaceable protected areas")
# toc()
# 
# cat("saving...")
# 
# st_save(sf=L_C4_Irrep_PAs, filename="L_C4_Irrep_PAs_polys.shp", outpath=output_path)
# 
# cat("done\n")

################################################################
#### KBAs (POLYGON) ############################################
################################################################

# cat("kbas...")
# 
# tic("kbas fix and union")
# kba_polys = filter(kba_aze_iba_polys, KbaStatus == "confirmed") %>%
#   dplyr::select(Criteria) %>%
#   fix_sf()
# 
# kba_pts = filter(kba_aze_iba_pts, kbastatus == "confirmed") %>%
#   mutate(sitarea = as.numeric(sitarea)) %>%
#   drop_na(sitarea)
# 
# kba_pts_buff = st_buffer_antimeridian(kba_pts, dist = sqrt((kba_pts$sitarea*10000)/pi), max_cells=5000) %>%
#   fix_sf() %>%
#   select(Criteria)
# 
# kba = bind_rows(kba_polys,kba_pts_buff)
# 
# L_C1_KBAs_A1ae = filter(kba, str_detect(Criteria,"A1a|A1e")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "KBAs under criteria A1a and A1e")
# 
# P_C1_KBAs_A1b = filter(kba, str_detect(Criteria,"A1b")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "KBAs under criterion A1b")
# 
# P_C1_KBAs_E = filter(kba, str_detect(Criteria,"E")) %>%
#  st_faster_union() %>%
#  mutate(Type = "Potential", Feature = "KBAs under criterion E")
# 
# L_C2_KBAs_B1 = filter(kba, str_detect(Criteria,"B1")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "KBAs under criterion B1")
# 
# L_C3_KBAs_D1a = filter(kba, str_detect(Criteria,"D1a")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "KBAs under criterion D1a")
# 
# P_C3_KBAs_D1b = filter(kba, str_detect(Criteria,"D1b")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "KBAs under criterion D1b")
# 
# L_C3_KBAs_D2 = filter(kba, str_detect(Criteria,"D2")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "KBAs under criterion D2")
# 
# P_C3_KBAs_D3 = filter(kba, str_detect(Criteria,"D3")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "KBAs under criterion D3")
# 
# L_C4_KBAs_A2a = filter(kba, str_detect(Criteria,"A2a")) %>%
#  st_faster_union() %>%
#  mutate(Type = "Likely", Feature = "KBAs under criterion A2a")
# 
# P_C4_KBAs_A2b = filter(kba, str_detect(Criteria,"A2b")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "KBAs under criterion A2b")
# 
# P_C4_KBAs_B4 = filter(kba, str_detect(Criteria,"B4")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "KBAs under criterion B4")
# 
# P_C4_KBAs_C = filter(kba, str_detect(Criteria,"C")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "KBAs under criterion C")
# 
# toc()
# 
# cat("saving...")
# 
# # criterion 1
# st_save(sf=L_C1_KBAs_A1ae, filename="L_C1_KBAs_A1ae_polys.shp", outpath=output_path)
# st_save(sf=P_C1_KBAs_A1b, filename="P_C1_KBAs_A1b_polys.shp", outpath=output_path)
# st_save(sf=P_C1_KBAs_E, filename="P_C1_KBAs_E_polys.shp", outpath=output_path)
# 
# # criterion 2
# st_save(sf=L_C2_KBAs_B1, filename="L_C2_KBAs_B1_polys.shp", outpath=output_path)
# 
# # criterion 3
# st_save(sf=L_C3_KBAs_D1a, filename="L_C3_KBAs_D1a_polys.shp", outpath=output_path)
# st_save(sf=P_C3_KBAs_D1b, filename="P_C3_KBAs_D1b_polys.shp", outpath=output_path)
# st_save(sf=L_C3_KBAs_D2, filename="L_C3_KBAs_D2_polys.shp", outpath=output_path)
# st_save(sf=P_C3_KBAs_D3, filename="P_C3_KBAs_D3_polys.shp", outpath=output_path)
# 
# # criterion 4
# st_save(sf=L_C4_KBAs_A2a, filename="L_C4_KBAs_A2a_polys.shp", outpath=output_path)
# st_save(sf=P_C4_KBAs_A2b, filename="P_C4_KBAs_A2b_polys.shp", outpath=output_path)
# st_save(sf=P_C4_KBAs_B4, filename="P_C4_KBAs_B4_polys.shp", outpath=output_path)
# st_save(sf=P_C4_KBAs_C, filename="P_C4_KBAs_C_polys.shp", outpath=output_path)
# 
# cat("done\n")

################################################################
#### AZEs (POLYGON) ############################################
################################################################

# cat("azes...")
# 
# tic("azes fix and union")
# aze_polys = filter(kba_aze_iba_polys, AzeStatus == "confirmed") %>%
#   fix_sf() %>%
#   dplyr::select(last_col())
# 
# aze_pts = filter(kba_aze_iba_pts, azestatus == "confirmed") %>% #error in dataset
#   mutate(sitarea = as.numeric(sitarea)) %>%
#   drop_na(sitarea) %>%
#   dplyr::select(sitarea,last_col())
# 
# aze_pts_buff = st_buffer_antimeridian(aze_pts, dist = sqrt((aze_pts$sitarea*10000)/pi), max_cells=5000) %>%
#   fix_sf() %>% 
#   dplyr::select(-sitarea)
# 
# L_C1_C2_C3_AZEs = bind_rows(aze_polys,aze_pts_buff) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Alliance for Zero Extinction Sites")
# toc()
# 
# cat("saving...")
# 
# st_save(sf=L_C1_C2_C3_AZEs, filename="L_C1_C2_C3_AZEs_polys.shp", outpath=output_path)
# 
# cat("done\n")

################################################################
#### IBAs (POLYGON) ############################################
################################################################

# cat("ibas...")
# 
# tic("ibas fix and union")
# iba_polys = st_read(paste0(data_path,"IBAsGlobal_2023_September_02/IBAsGlobal_2023_September_POL_02.shp"), quiet=TRUE) %>%
#   dplyr::select(SitRecID) %>%
#   fix_sf()
# 
# iba_pts = st_read(paste0(data_path,"IBAsGlobal_2023_September_02/IBAsGlobal_2023_September_PNT_02.shp"), quiet=TRUE) %>%
#   drop_na(SitArea)
# 
# iba_pts_buff = st_buffer_antimeridian(iba_pts, dist = sqrt((iba_pts$SitArea*10000)/pi), max_cells=5000) %>%
#   fix_sf() %>%
#   select(SitRecID)
# 
# iba = bind_rows(iba_polys,iba_pts_buff)
# 
# iba_crit_lookup = read.csv(paste0(data_path,"IBAsGlobal_2023_September_02/TriggerSpecies.csv"))
# 
# iba = left_join(iba,iba_crit_lookup,by="SitRecID")
# 
# L_C1_IBAs_A1 = filter(iba, str_detect(SitCriSumConfirmed,"A1")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "IBAs under criterion A1")
# 
# P_C1_IBAs_B1b = filter(iba, str_detect(SitCriSumConfirmed,"B1b")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "IBAs under criterion B1b")
# 
# P_C2_IBAs_A2 = filter(iba, str_detect(SitCriSumConfirmed,"A2")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "IBAs under criterion A2")
# 
# L_C3_IBAs_A4 = filter(iba, str_detect(SitCriSumConfirmed,"A4")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "IBAs under criterion A4")
# 
# P_C4_IBAs_A3 = filter(iba, str_detect(SitCriSumConfirmed,"A3")) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "IBAs under criterion A3")
# 
# toc()
# 
# cat("saving...")
# 
# st_save(sf=L_C1_IBAs_A1, filename="L_C1_IBAs_A1_polys.shp", outpath=output_path)
# st_save(sf=P_C1_IBAs_B1b, filename="P_C1_IBAs_B1b_polys.shp", outpath=output_path)
# st_save(sf=P_C2_IBAs_A2, filename="P_C2_IBAs_A2_polys.shp", outpath=output_path)
# st_save(sf=L_C3_IBAs_A4, filename="L_C3_IBAs_A4_polys.shp", outpath=output_path)
# st_save(sf=P_C4_IBAs_A3, filename="P_C4_IBAs_A3_polys.shp", outpath=output_path)
# 
# cat("done\n")

################################################################
#### RAMSAR (POINT & POLY) #####################################
################################################################

cat("ramsar wetlands...")

tic("ramsar sites fix and union")
ramsar_polys = filter(wdpa_polys, STATUS %ni% c("Proposed","Not Reported") &
                        DESIG_ENG == "Ramsar Site, Wetland of International Importance" &
                        DESIG_TYPE == "International") %>%
  dplyr::select(REP_AREA,INT_CRIT) %>%
  fix_sf()

ramsar_pts = filter(wdpa_pts, STATUS %ni% c("Proposed","Not Reported") &
                      DESIG_ENG == "Ramsar Site, Wetland of International Importance" &
                      DESIG_TYPE == "International") %>%
  filter(REP_AREA > 0) %>%
  dplyr::select(REP_AREA,INT_CRIT)

ramsar_pts_buff = st_buffer_antimeridian(sf=ramsar_pts, dist=sqrt((ramsar_pts$REP_AREA*1000000)/pi),max_cells=5000) %>%
  fix_sf()

ramsar = bind_rows(ramsar_polys,ramsar_pts_buff)

L_C1_Ramsar_ii = filter(ramsar, str_detect(INT_CRIT,"(ii)")) %>%
  st_faster_union() %>%
  mutate(Type = "Likely", Feature = "Ramsar sites under criterion 2")

L_C3_Ramsar_v_vi = filter(ramsar, str_detect(INT_CRIT,"(v)|(vi)")) %>%
  st_faster_union() %>%
  mutate(Type = "Likely", Feature = "Ramsar sites under criteria 5 and 6")

L_C4_Ramsar_i_iii = filter(ramsar, str_detect(INT_CRIT,"(i)|(iii)")) %>%
  st_faster_union() %>%
  mutate(Type = "Likely", Feature = "Ramsar sites under criteria 1 and 3")

P_C3_Ramsar_iv_vii_viii_ix = filter(ramsar, str_detect(INT_CRIT,"(iv)|(vii)|(viii)|(ix)")) %>%
  st_faster_union() %>%
  mutate(Type = "Potential", Feature = "Ramsar sites under criteria 4, 7, 8 and 9")

L_C4_All_Ramsar = ramsar %>%
  st_faster_union() %>%
  mutate(Type = "Likely", Feature = "All Ramsar sites")
toc()

cat("saving...")

st_save(sf=L_C1_Ramsar_ii, filename="L_C1_Ramsar_ii_polys.shp", outpath=output_path)
st_save(sf=L_C3_Ramsar_v_vi, filename="L_C3_Ramsar_v_vi_polys.shp", outpath=output_path)
st_save(sf=L_C4_Ramsar_i_iii, filename="L_C4_Ramsar_i_iii_polys.shp", outpath=output_path)
st_save(sf=P_C3_Ramsar_iv_vii_viii_ix, filename="P_C3_Ramsar_iv_vii_viii_ix_polys.shp", outpath=output_path)
st_save(sf=L_C4_All_Ramsar, filename="L_C4_All_Ramsar_polys.shp", outpath=output_path)

cat("done\n")

################################################################
#### IUCN I/II PROTECTED AREAS (POLYGON) #######################
################################################################

cat("iucn i/ii PAs...")

tic("iucn i/ii PAs fix and union")
pa_polys = filter(wdpa_polys, STATUS %ni% c("Proposed","Not Reported") &
                    IUCN_CAT %in% c("Ia","Ib","II")) %>%
  dplyr::select(REP_AREA) %>%
  fix_sf()

pa_pts = filter(wdpa_pts, STATUS %ni% c("Proposed","Not Reported") &
                  IUCN_CAT %in% c("Ia","Ib","II")) %>%
  filter(REP_AREA > 0) %>%
  dplyr::select(REP_AREA)

pa_pts_buff = st_buffer_antimeridian(pa_pts,dist = sqrt((pa_pts$REP_AREA*1000000)/pi), max_cells=5000) %>%
  fix_sf()

L_C4_IUCN_Ia_Ib_II = bind_rows(pa_polys,pa_pts_buff) %>%
  st_faster_union() %>%
  mutate(Type = "Likely", Feature = "IUCN management categories Ia, Ib and II")
toc()

cat("saving...")

st_save(sf=L_C4_IUCN_Ia_Ib_II, filename="L_C4_IUCN_Ia_Ib_II_polys.shp", outpath=output_path)

cat("done\n")

################################################################
#### WORLD HERITAGE SITES (POLYGON) ############################
################################################################

cat("world heritage sites...")

tic("world heritage sites fix and union")
whs = filter(wdpa_polys, STATUS %ni% c("Proposed","Established","Not Reported") &
               DESIG_ENG == "World Heritage Site (natural or mixed)" &
               DESIG_TYPE == "International") %>%
  fix_sf()

L_C4_WHS = whs %>%
  st_faster_union() %>%
  mutate(Type = "Likely", Feature = "Natural and mixed World Heritage sites")
toc()

cat("saving...")

st_save(sf=L_C4_WHS, filename="L_C4_WHS_polys.shp", outpath=output_path)

cat("done\n")

################################################################
#### SALTMARSH (POINT & POLY) ##################################
################################################################

# cat("saltmarshes...")
# 
# L_C4_Saltmarsh_pts = st_read(paste0(data_path,"WCMC027_Saltmarsh_v6/01_Data/WCMC027_Saltmarshes_Pt_v6.shp"), quiet=TRUE) %>%
#   st_faster_union() %>%
#   st_zm() %>%
#   mutate(Type = "Likely", Feature = "Saltmarshes")
# 
# toc("saltmarsh polys read in, fix and union")
# L_C4_Saltmarsh_polys = st_read(paste0(data_path,"WCMC027_Saltmarsh_v6/01_Data/WCMC027_Saltmarshes_Py_v6.shp"), quiet=TRUE) %>%
#   fix_sf() %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Saltmarshes")
# toc()
# 
# cat("saving...")
# 
# st_save(sf=L_C4_Saltmarsh_pts, filename="L_C4_Saltmarsh_pts.shp", outpath=output_path)
# st_save(sf=L_C4_Saltmarsh_polys, filename="L_C4_Saltmarsh_polys.shp", outpath=output_path)
# 
# cat("done\n")

################################################################
#### SEAGRASS (POINT & POLY) ###################################
################################################################

# cat("seagrass...")
# 
# L_C4_Seagrass_pts = st_read(paste0(data_path,"WCMC013-014_SeagrassPtPy2021_v7_1/01_Data/WCMC_013_014_SeagrassesPt_v7_1.shp"), quiet=TRUE) %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Seagrass beds")
# 
# tic("seagrass polys read in, fix and union")
# L_C4_Seagrass_polys = st_read(paste0(data_path,"WCMC013-014_SeagrassPtPy2021_v7_1/01_Data/WCMC013014-Seagrasses-Py-v7_1.shp"), quiet=TRUE) %>%
#   st_transform(4326) %>%
#   fix_sf() %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Seagrass beds")
# toc()
# 
# cat("saving...")
# 
# st_save(sf=L_C4_Seagrass_pts, filename="L_C4_Seagrass_pts.shp", outpath=output_path)
# st_save(sf=L_C4_Seagrass_polys, filename="L_C4_Seagrass_polys.shp", outpath=output_path)
# 
# cat("done\n")

################################################################
#### SEAMOUNTS (POINTS) ########################################
################################################################

# cat("seamounts...")
# 
# P_C4_Seamounts = st_read(paste0(data_path,"ZSL-002-ModelledSeamounts2011/DownloadPack-14_001_ZSL002_ModelledSeamounts2011_v1/01_Data/Seamounts/Seamounts.shp"), quiet=TRUE) %>%
#   st_faster_union() %>%
#   mutate(Type = "Potential", Feature = "Seamounts")
# 
# cat("saving...")
# 
# st_save(sf=P_C4_Seamounts, filename="P_C4_Seamounts_pts.shp", outpath=output_path)
# 
# cat("done\n")

################################################################
#### TIGER CONSERVATION LANDSCAPES (POLYGON) ###################
################################################################

# cat("tiger conservation landscapes...")
# 
# tic("tiger conservation landscapes read in, fix and union")
# L_C1_Tiger = st_read(paste0(data_path,"Tiger_Conservation_Landscapes/Tiger_Conservation_Landscapes.shp"), quiet=TRUE) %>%
#   filter(tx2_tcl==0) %>%
#   fix_sf() %>%
#   st_faster_union() %>%
#   mutate(Type = "Likely", Feature = "Tiger Conservation Landscapes")
# toc()
# 
# cat("saving...")
# 
# st_save(sf=L_C1_Tiger, filename="L_C1_Tiger_polys.shp", outpath=output_path)
# 
# cat("done\n")