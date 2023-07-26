################################################################
#### PREPROCESSING NON-RED LIST DATA (VECTORS) #################
################################################################

# Author: Seb Dunnett
# Created: 20/07/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,tidyverse,terra,tictoc)

data_path = "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/"

output_path = "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

wdpa_gdb = paste0(data_path,"WDPA_Dec2022_Licensed.gdb")

wdpa_poly_layer = st_layers(wdpa_gdb)$name %>% keep(.,str_detect(.,"poly"))
wdpa_pt_layer = st_layers(wdpa_gdb)$name %>% keep(.,str_detect(.,"point"))

wdpa_polys = st_read(wdpa_gdb, layer = wdpa_poly_layer)
wdpa_pts = st_read(wdpa_gdb, layer = wdpa_pt_layer)

kba_polys = st_read(dsn = paste0(data_path,"KBAsGlobal_2022_03/KBAsGlobal_2022_03/KBAsGlobal_2022_03_POL.shp"))
kba_pts = st_read(dsn = paste0(data_path,"KBAsGlobal_2022_03/KBAsGlobal_2022_03/KBAsGlobal_2022_03_PNT.shp"))

`%ni%` = Negate(`%in%`)

################################################################
#### SWOT TURTLE NESTS #########################################
################################################################

swot = st_read(paste0(data_path,"obis_seamap_swot/obis_seamap_swot_5f7dd60721f10_20201007_105234_site_locations_shapefile.shp"))

L_C1_Turtle_CREN = filter(swot,commonname %in% c("Green Sea Turtle","Hawksbill Sea Turtle","Kemp's Ridley")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Sea turtle nesting sites - CR and EN species",
         C1=1,C2=0,C3=0,C4=0,C5=0)

P_C3_C4_Turtle_all = swot %>%
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "Sea turtle nesting sites - All species",
         C1=0,C2=0,C3=1,C4=1,C5=0)
  
################################################################
#### CLOUD FORESTS #############################################
################################################################

P_C4_Cloud_Forest = st_read(paste0(data_path,"cloud_forests/cloud_forest_points_1997.shp")) %>%
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "Tropical montane cloud forests",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### COLD SEEPS ################################################
################################################################

cold_seeps = st_read(paste0(data_path,"ChEssBase_20200910_v1_1/ChEssBase_20200910_v1_1_occurrence.shp"))

L_C4_C5_Cold_seeps = cold_seeps %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Cold seeps",
         C1=0,C2=0,C3=0,C4=0,C5=1)

P_C2_Cold_seeps = cold_seeps %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "Cold seeps",
         C1=0,C2=1,C3=0,C4=0,C5=0)

################################################################
#### OBSERVED COLDWATER CORAL ##################################
################################################################

cw_coral_pts = st_read(paste0(data_path,"14_001_WCMC001_ColdCorals2017_v5_1/01_Data/WCMC001_ColdCorals2017_Pt_v5_1.shp"))

cw_coral_polys = st_read(paste0(data_path,"14_001_WCMC001_ColdCorals2017_v5_1/01_Data/WCMC001_ColdCorals2017_Py_v5_1.shp")) %>% 
  dplyr::select(-REP_AREA_K) #prevents binding as chr vs num

L_C4_C5_Cold_water_coral_observed = bind_rows(cw_coral_pts,cw_coral_polys) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Cold water coral reefs - Observed occurence",
         C1=0,C2=0,C3=0,C4=1,C5=1)

################################################################
#### CORAL REEFS ###############################################
################################################################

cr_pts = st_read(paste0(data_path,"14_001_WCMC008_CoralReefs2018_v4_1/01_Data/WCMC008_CoralReef2018_Pt_v4_1.shp"))

cr_polys = st_read(paste0(data_path,"14_001_WCMC008_CoralReefs2018_v4_1/01_Data/WCMC008_CoralReef2018_Py_v4_1.shp"))

L_C4_C5_Warm_water_coral = bind_rows(cr_pts,cr_polys) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Warm water coral reefs",
         C1=0,C2=0,C3=0,C4=1,C5=1)

################################################################
#### EBSAs #####################################################
################################################################

ebsa = st_read(paste0(data_path,"CBD-001-EBSAs/02_Data_records/Global_EBSAs_Critieria_Join_WGS84.shp"))

ebsa_c1 = filter(ebsa, Endangered == "H")

ebsa_c3 = filter(ebsa, Life_Histo == "H")

ebsa_c4 = filter(ebsa, Unique_Rar == "H" | Fragility == "H" | Naturalnes == "H")

################################################################
#### HYDROTHERMAL VENTS ########################################
################################################################

hydrothermal_vents = st_read(paste0(data_path,"PANGEA_2020_InterRidge_Database_Hydrothermal_Vent_v3_4/vent_fields_all_20200325.shp"))

L_C2_C5_Hydrothermal_Vents = hydrothermal_vents %>%
  filter(Activity %in% c("active, inferred","active, confirmed")) %>%
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Hydrothermal Vents",
         C1=0,C2=1,C3=0,C4=0,C5=1)

P_C4_Hydrothermal_Vents = hydrothermal_vents %>%
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "Hydrothermal Vents",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### IMMAs #####################################################
################################################################

imma = st_read(paste0(data_path,"iucn-imma/iucn-imma_oct20.shp"))

P_C1_IMMAs_A = filter(imma, str_detect(Criteria.2,"A")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "IMMAs under criterion A",
         C1=1,C2=0,C3=,C4=0,C5=0)
 
P_C2_IMMAs_B1 = filter(imma, str_detect(Criteria.2,"B1")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "IMMAs under criterion B1",
         C1=0,C2=1,C3=0,C4=0,C5=0)

L_C3_IMMAs_B2 = filter(imma, str_detect(Criteria.2,"B2")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "IMMAs under criterion B2",
         C1=0,C2=0,C3=1,C4=0,C5=0)
 
P_C3_IMMAs_C1_C2_C3 = filter(imma, str_detect(Criteria.2,"C1|C2|C3")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "IMMAs under criteria C1, C2 and C3",
         C1=0,C2=0,C3=1,C4=0,C5=0)
 
P_C2_C4_IMMAs_D1 = filter(imma, str_detect(Criteria.2,"D1")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "IMMAs under criterion D1",
         C1=0,C2=1,C3=0,C4=1,C5=0)

P_C4_IMMAs_D2 = filter(imma, str_detect(Criteria.2,"D2")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "IMMAs under criterion D2",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### INTACT FOREST LANDSCAPES ##################################
################################################################

L_C4_Intact_Forest_Landscapes = st_read(paste0(data_path,"IFL_2016/ifl_2016.shp")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Intact Forest Landscapes",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### IRREPLACEABLE PAs #########################################
################################################################

L_C4_Irrep_PAs = st_read(paste0(data_path,"Irreplaceable/irrep_pa_WDPA_poly_Nov2022.shp")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Irreplaceable protected areas",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### KBAs ######################################################
################################################################

kba_polys = filter(kba_polys, KBASTATUS == "confirmed") %>% 
  dplyr::select(Triggers)

kba_pts = filter(kba_pts, KBASTATUS == "confirmed") %>% 
  st_transform("ESRI:54009") %>% 
  filter(SitArea > 0) %>% 
  dplyr::select(Triggers,SitArea)

kba_pts_buff = st_buffer(kba_pts, dist = sqrt((kba_pts$SitArea*10000)/pi)) %>% 
  st_transform(4326)

kba = bind_rows(kba_polys,kba_pts_buff)

L_C1_KBAs_CREN = filter(kba, str_detect(Triggers,"CR/EN")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "KBAs with triggers including CR/EN species",
         C1=1,C2=0,C3=0,C4=0,C5=0)

P_C1_KBAs_VU = filter(kba, str_detect(Triggers,"VU")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "KBAs with triggers including VU species",
         C1=1,C2=0,C3=0,C4=0,C5=0)

L_C2_KBAs_endemic = filter(kba, str_detect(Triggers,"endemic")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "KBAs with triggers including endemic species",
         C1=0,C2=1,C3=0,C4=0,C5=0)

L_C3_KBAs_migratory = filter(kba, str_detect(Triggers,"migratory birds/congregation")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "KBAs with triggers including migratory birds/congregations",
         C1=0,C2=0,C3=1,C4=0,C5=0)

L_C4_KBAs_other = filter(kba, str_detect(Triggers,"other")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "KBAs with triggers including other",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### AZEs ######################################################
################################################################

aze_polys = filter(kba_polys, AZESTATUS == "confirmed") %>% 
  dplyr::select(SitArea)

aze_pts = filter(kba_pts, AZESTATUS == "confirmed") %>%
  st_transform("ESRI:54009") %>% 
  filter(SitArea > 0) %>% 
  dplyr::select(SitArea)

aze_pts_buff = st_buffer(aze_pts, dist = sqrt((aze_pts$SitArea*10000)/pi)) %>% 
  st_transform(4326)

L_C1_C2_C3_AZEs = bind_rows(aze_polys,aze_pts_buff) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Alliance for Zero Extinction Sites",
         C1=1,C2=1,C3=1,C4=0,C5=0)

################################################################
#### IBAs ######################################################
################################################################

iba_polys = filter(kba_polys, IBASTATUS = "confirmed") %>% 
  dplyr::select(Triggers)

iba_pts = filter(kba_pts, IBASTATUS = "confirmed") %>% 
  st_transform("ESRI:54009") %>% 
  filter(SitArea > 0) %>% 
  dplyr::select(Triggers,SitArea)

iba_pts_buff = st_buffer(iba_pts, dist = sqrt((iba_pts$SitArea*10000)/pi)) %>% 
  st_transform(4326)

iba = bind_rows(iba_polys,iba_pts_buff)

iba_CREN = filter(iba, str_detect(Triggers,"CR/EN"))

iba_VU = filter(iba, str_detect(Triggers,"VU"))

iba_endemic = filter(iba, str_detect(Triggers,"endemic"))

iba_congregation = filter(iba, str_detect(Triggers,"migratory birds/congregation"))

################################################################
#### MANGROVES #################################################
################################################################

L_C4_Mangrove = st_read(paste0(data_path,"GMW_v2/01_Data/GMW_2016_v2.shp")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Mangroves",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### RAMSAR ####################################################
################################################################

ramsar_polys = filter(wdpa_poly, STATUS %ni% c("Proposed","Established","Not Reported") &
                        DESIG_ENG == "Ramsar Site, Wetland of International Importance" &
                        DESIG_TYPE == "International") %>% 
  dplyr::select(REP_AREA,INT_CRIT)

ramsar_pts = filter(wdpa_pts, STATUS %ni% c("Proposed","Established","Not Reported") &
                      DESIG_ENG == "Ramsar Site, Wetland of International Importance" &
                      DESIG_TYPE == "International") %>%
  st_transform("ESRI:54009") %>% 
  filter(REP_AREA > 0) %>% 
  dplyr::select(REP_AREA,INT_CRIT)

ramsar_pts_buff = st_buffer(ramsar_pts, dist = sqrt((ramsar_pts$REP_AREA*1000000)/pi)) %>% 
  st_transform(4326)

ramsar = bind_rows(ramsar_polys,ramsar_pts_buff)

L_C1_Ramsar_ii = filter(ramsar, str_detect(INT_CRIT,"(ii)")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Ramsar sites under criterion 2",
         C1=1,C2=0,C3=0,C4=0,C5=0)

L_C3_Ramsar_v_vi = filter(ramsar, str_detect(INT_CRIT,"(v)|(vi)")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Ramsar sites under criteria 5 and 6",
         C1=0,C2=0,C3=1,C4=0,C5=0)

L_C4_Ramsar_i_iii = filter(ramsar, str_detect(INT_CRIT,"(i)|(iii)")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Ramsar sites under criteria 1 and 3",
         C1=0,C2=0,C3=0,C4=1,C5=0)

P_C3_Ramsar_iv_vii_viii_ix = filter(ramsar, str_detect(INT_CRIT,"(iv)|(vii)|(viii)|(ix)")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "Ramsar sites under criteria 4, 7, 8 and 9",
         C1=0,C2=0,C3=1,C4=0,C5=0)

L_C4_All_Ramsar = ramsar %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "All Ramsar sites",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### IUCN I/II PROTECTED AREAS #################################
################################################################

pa_polys = filter(wdpa_poly, STATUS %ni% c("Proposed","Established","Not Reported") &
                    IUCN_CAT %in% c("Ia","Ib","II")) %>% 
  dplyr::select(REP_AREA)

pa_pts = filter(wdpa_pts, STATUS %ni% c("Proposed","Established","Not Reported") &
                  IUCN_CAT %in% c("Ia","Ib","II")) %>%
  st_transform("ESRI:54009") %>% 
  filter(REP_AREA > 0) %>% 
  dplyr::select(REP_AREA)

pa_pts_buff = st_buffer(pa_pts, dist = sqrt((pa_pts$REP_AREA*1000000)/pi)) %>% 
  st_transform(4326)

L_C4_IUCN_Ia_Ib_II = bind_rows(pa_polys,pa_pts_buff) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "IUCN management categories Ia, Ib and II",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### WORLD HERITAGE SITES ######################################
################################################################

whs = filter(wdpa_poly, STATUS %ni% c("Proposed","Established","Not Reported") &
               DESIG_ENG == "World Heritage Site (natural or mixed)" &
               DESIG_TYPE == "International")

L_C4_WHS = whs %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Natural and mixed World Heritage sites",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### SALTMARSH #################################################
################################################################

saltmarsh_pts = st_read(paste0(data_path,"WCMC027_Saltmarsh_v6/01_Data/WCMC027_Saltmarshes_Pt_v6.shp")) %>% 
  st_geometry() %>% st_sf(sf_column_name = "geometry")

saltmarsh_polys = st_read(paste0(data_path,"WCMC027_Saltmarsh_v6/01_Data/WCMC027_Saltmarshes_Py_v6.shp")) %>% 
  st_geometry() %>% st_sf(sf_column_name = "geometry")

L_C4_Saltmarsh = bind_rows(saltmarsh_polys,saltmarsh_pts) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Saltmarshes",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### SEAGRASS ##################################################
################################################################

seagrass_pts = st_read(paste0(data_path,"WCMC013-014_SeagrassPtPy2021_v7_1/01_Data/WCMC_013_014_SeagrassesPt_v7_1.shp")) %>% 
  st_geometry() %>% st_sf(sf_column_name = "geometry")

seagrass_polys = st_read(paste0(data_path,"WCMC013-014_SeagrassPtPy2021_v7_1/01_Data/WCMC013014-Seagrasses-Py-v7_1.shp")) %>% 
  st_geometry() %>% st_sf(sf_column_name = "geometry")

L_C4_Seagrass = bind_rows(seagrass_polys,seagrass_pts) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Seagrass beds",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### SEAMOUNTS #################################################
################################################################

P_C4_Seamounts = st_read(paste0(data_path,"ZSL-002-ModelledSeamounts2011\DownloadPack-14_001_ZSL002_ModelledSeamounts2011_v1\01_Data\Seamounts\Seamounts.shp")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Potential", Feature = "Seamounts",
         C1=0,C2=0,C3=0,C4=1,C5=0)

################################################################
#### TIGER CONSERVATION LANDSCAPES #############################
################################################################

L_C1_Tiger = st_read(paste0(data_path,"Tiger_Conservation_Landscapes/Tiger_Conservation_Landscapes.shp")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type = "Likely", Feature = "Tiger Conservation Landscapes",
         C1=1,C2=0,C3=0,C4=0,C5=0)

################################################################
#### COMBINE VECTORS ###########################################
################################################################

Likely_Critical_Habitat = rbind(
  L_C1_C2_C3_AZEs,
  L_C1_KBAs_CREN,
  L_C1_Ramsar_ii,
  L_C1_Tiger,
  L_C1_Turtle_CREN,
  L_C2_C5_Hydrothermal_Vents,
  L_C2_KBAs_endemic,
  L_C3_IMMAs_B2,
  L_C3_KBAs_migratory,
  L_C3_Ramsar_v_vi,
  L_C4_All_Ramsar,
  L_C4_C5_Cold_seeps,
  L_C4_C5_Cold_water_coral_observed,
  L_C4_C5_Warm_water_coral,
  L_C4_IUCN_Ia_Ib_II,
  L_C4_Intact_Forest_Landscapes,
  L_C4_Irrep_PAs,
  L_C4_KBAs_other,
  L_C4_Mangrove,
  L_C4_Ramsar_i_iii,
  L_C4_Saltmarsh,
  L_C4_Seagrass,
  L_C4_WHS
)

Potential_Critical_Habitat = rbind(
  P_C1_IMMAs_A,
  P_C1_KBAs_VU,
  P_C2_C4_IMMAs_D1,
  P_C2_Cold_seeps,
  P_C2_IMMAs_B1,
  P_C3_C4_Turtle_all,
  P_C3_IMMAs_C1_C2_C3,
  P_C3_Ramsar_iv_vii_viii_ix,
  P_C4_Cloud_Forest,
  P_C4_Hydrothermal_Vents,
  P_C4_IMMAs_D2,
  P_C4_Seamounts
)

################################################################
#### WRITE GPKGS ###############################################
################################################################

# check output folder for previously made files
# rename old files (and delete older)
# save
likely_file = paste0(output_path,"Likely_Critical_Habitat_vectors.gpkg")
potential_file = paste0(output_path,"Likely_Critical_Habitat_vectors.gpkg")

output_files = c(likely_file,potential_file)

if((output_files %in% list.files(output_path, full.names = TRUE) %>% sum)>1){
  file.remove(str_replace(output_files,".gpkg","_old.gpkg"))
  file.rename(output_files,str_replace(output_files,".gpkg","_old.gpkg"))
} else{}

cat("saving\n")
st_write(Likely_Critical_Habitat,likely_file)
st_write(Likely_Potential_Habitat,potential_file)