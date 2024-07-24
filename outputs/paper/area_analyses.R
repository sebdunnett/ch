# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,terra,sf,giscoR,scico,tictoc)
terraOptions(memfrac=0.9)

wrld_eez = st_read("raw_data/UN map borders/GADM_EEZ.gpkg") |> st_transform("ESRI:54012") |>
  st_simplify(dTolerance=1000) |>
  st_transform(4326)

ch2023 = rast("outputs/Basic_Critical_Habitat_Raster_WGS.tif")

ch2018 = rast("crhab_lpu") |>
  classify(rbind(c(1,0),c(2,10),c(3,1)))

crs(ch2018) = "ESRI:102100"

ch2018 = project(ch2018, ch2023, method="near")

cell_areas = cellSize(ch2023,unit="km")

tic("calculate CH changes")
ch_changes = c(ch2018==1 & ch2023==10,
               ch2018==0 & ch2023==1,
               ch2018==0 & ch2023==10,
               ch2018==10 & ch2023==1,
               ch2018==1 & ch2023==0,
               ch2018==10 & ch2023==0)
toc()

ch_upgrade = ch_changes[[1]]
ch_addition = app(ch_changes[[2:3]],any)
ch_downgrade = ch_changes[[4]]
ch_removal = app(ch_changes[[5:6]],any)

ch_changes_out = c(ch_upgrade,ch_addition,ch_downgrade,ch_removal)*c(1,2,-1,-2)
ch_changes_out = app(ch_changes_out,sum)

tic("segregate layers")
ch2023_binary = segregate(ch2023)
names(ch2023_binary) <- c("Unclassified2023","Potential2023","Likely2023")
ch2023_binary = ch2023_binary * cell_areas

ch2018_binary = segregate(ch2018)
names(ch2018_binary) <- c("Unclassified2018","Potential2018","Likely2018")
ch2018_binary = ch2018_binary * cell_areas

ch_changes_binary = segregate(ch_changes_out)
names(ch_changes_binary) <- c("Removal","Downgrade","No change","Upgrade","Addition")
ch_changes_binary = ch_changes_binary * cell_areas
toc()

wrldr = rasterize(vect(wrld_eez),ch2023,field="objectid",touches=TRUE)
wrldr_area = expanse(wrldr,unit="km",byValue=TRUE) |>
  select(-layer) |>
  rename(zone=value,total_country_area_km2=area)
ch_areas = expanse(c(ch2023,ch2018,ch_changes_out),unit="km",zones=wrldr,byValue=TRUE) |>
  left_join(wrldr_area) |>
  mutate(layer=case_match(layer,
                          1 ~ "2023",
                          2 ~ "2018",
                          3 ~ "ch"),
         value=case_when(
           layer!="ch" & value==0 ~ "Unclassified",
           layer!="ch" & value==1 ~ "Potential",
           layer!="ch" & value==10 ~ "Likely",
           layer=="ch" & value==-2 ~ "Removal",
           layer=="ch" & value==-1 ~ "Downgrade",
           layer=="ch" & value==0 ~ "NoChange",
           layer=="ch" & value==1 ~ "Upgrade",
           layer=="ch" & value==2 ~ "Addition"),
         name=paste(layer,value,sep="_")) |>
  select(-layer,-value) |>
  pivot_wider(id_cols=c("zone","total_country_area_km2"),names_from="name",values_from="area")

ch_areas_pct = ch_areas |>
  mutate(across(3:13, ~ .x/total_country_area_km2))

write.csv(ch_areas,"O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/paper/wrld_ch_areas.csv", row.names = FALSE)
write.csv(ch_areas_pct,"O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/paper/wrld_ch_areas_pct.csv", row.names = FALSE)

ch2023_drill_down = rast("outputs/Critical_Habitat_Drill_Down_WGS.tif")
activeCat(ch2023_drill_down) <- "VALUE"

df = cats(ch2023_drill_down)[[1]]

ch2023_drill_down_areas = expanse(ch2023_drill_down,unit="km",byValue=TRUE) |>
  select(-layer) |>
  mutate(value=as.numeric(value))

df = left_join(df,ch2023_drill_down_areas,by=c("VALUE"="value"))

ch_addition_mask = mask(ch2023_drill_down,ch_addition,maskvalue=0)
ch_addition_drill_down = expanse(ch_addition_mask,unit="km",byValue=TRUE)[,2:3] |>
  rename(ADDITION=area)

ch_upgrade_mask = mask(ch2023_drill_down,ch_upgrade,maskvalue=0)
ch_upgrade_drill_down = expanse(ch_upgrade_mask,unit="km",byValue=TRUE)[,2:3] |>
  rename(UPGRADE=area)

ch_downgrade_mask = mask(ch2023_drill_down,ch_downgrade,maskvalue=0)
ch_downgrade_drill_down = expanse(ch_downgrade_mask,unit="km",byValue=TRUE)[,2:3] |>
  rename(DOWNGRADE=area)

ch_removal_mask = mask(ch2023_drill_down,ch_removal,maskvalue=0)
ch_removal_drill_down = expanse(ch_removal_mask,unit="km",byValue=TRUE)[,2:3] |>
  rename(REMOVAL=area)

ch_changes_df = reduce(list(ch_addition_drill_down,
           ch_upgrade_drill_down,
           ch_downgrade_drill_down), full_join, by = "value") |>
  mutate(value=as.numeric(value)) |>
  mutate(across(2:4, ~ replace_na(.x,0)))

df = left_join(df,ch_changes_df,by=c("VALUE"="value"))

df_pivot = pivot_longer(df,starts_with(c("L_","P_")),names_to="Feature",values_to="Feature_Present")

filter(df_pivot,Feature_Present==1) |>
  group_by(Feature) |>
  summarise(add_area=sum(ADDITION,na.rm=TRUE)) |>
  mutate(total_add_area=expanse(ch_addition_mask,unit="km")[1,2],
         pct=add_area/total_add_area) |>
  slice_max(pct,n=10)

filter(df_pivot,Feature_Present==1) |>
  group_by(Feature) |>
  summarise(upgr_area=sum(UPGRADE,na.rm=TRUE)) |>
  mutate(total_upgr_area=expanse(ch_upgrade_mask,unit="km")[1,2],
         pct=upgr_area/total_upgr_area) |>
  slice_max(pct,n=10)

filter(df_pivot,Feature_Present==1) |>
  group_by(Feature) |>
  summarise(downgr_area=sum(DOWNGRADE,na.rm=TRUE)) |>
  mutate(total_downgr_area=expanse(ch_downgrade_mask,unit="km")[1,2],
         pct=downgr_area/total_downgr_area) |>
  slice_max(pct,n=10)

features_count = filter(df_pivot,Feature_Present==1) |>
     group_by(VALUE) |>
     summarise(count=n())

ch2023_drill_down_stack = mask(ch2023_drill_down,ch2023_binary,maskvalue=0)[[2:3]]
names(ch2023_drill_down_stack) <- c("Potential","Likely")

features_count_stack = classify(ch2023_drill_down_stack,features_count)
