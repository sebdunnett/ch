# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,terra,sf,giscoR,scico,tictoc)
terraOptions(memfrac=0.9)

output_path = "outputs/"

cat("read in UN borders\n")
tic()
wrld_eez = st_read("raw_data/UN map borders/GADM_EEZ.gpkg",quiet=TRUE) |> st_transform("ESRI:54012") |>
  st_simplify(dTolerance=1000) |>
  st_transform(4326)
toc()

ch_basic_files = list.files(output_path, pattern = "Basic_Critical_Habitat.*\\.tif$", full.names=TRUE)
ch_basic_file = ch_basic_files[which.max(file.info(ch_basic_files)$ctime)]
ch2024 = rast(ch_basic_file)

ch2018 = rast("outputs/paper/ch2018_30as.tif")

cell_areas = cellSize(ch2024,unit="km")

cat("calculate CH changes\n")
tic()
ch_changes = ch2024-ch2018
ch_changes = classify(ch_changes, rbind(c(-10,-2),
                 c(-9,-1),
                 c(-1,-2),
                 c(1,2),
                 c(9,1),
                 c(10,2)))
toc()

cat("segregate layers\n")
tic()
ch2024_binary = segregate(ch2024)
names(ch2024_binary) <- c("Unclassified2024","Potential2024","Likely2024")
ch2024_binary = ch2024_binary * cell_areas

ch2018_binary = segregate(ch2018)
names(ch2018_binary) <- c("Unclassified2018","Potential2018","Likely2018")
ch2018_binary = ch2018_binary * cell_areas

ch_changes_binary = segregate(ch_changes)
names(ch_changes_binary) <- c("Removal","Downgrade","No change","Upgrade","Addition")
ch_changes_binary = ch_changes_binary * cell_areas
toc()

cat("calculate country areas\n")
tic()
wrldr = rasterize(vect(wrld_eez),ch2024,field="objectid",touches=TRUE)
wrldr_area = expanse(wrldr,unit="km",byValue=TRUE) |>
  select(-layer) |>
  rename(zone=value,total_country_area_km2=area)
ch_areas = expanse(c(ch2024,ch2018,ch_changes),unit="km",zones=wrldr,byValue=TRUE) |>
  left_join(wrldr_area) |>
  mutate(layer=case_match(layer,
                          1 ~ "2024",
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

ch_areas = left_join(st_drop_geometry(wrld_eez),ch_areas,by=c("objectid"="zone"))

ch_areas_pct = ch_areas |>
  mutate(across("2024_Unclassified":"ch_Addition", ~ .x/total_country_area_km2))
toc()

write.csv(ch_areas,"outputs/paper/wrld_ch_areas.csv", row.names = FALSE)
write.csv(ch_areas_pct,"outputs/paper/wrld_ch_areas_pct.csv", row.names = FALSE)

writeRaster(ch_changes,"outputs/paper/changes_2018_2024.tif",overwrite=TRUE)

cat("read in drill down rasters\n")
tic()
ch_drill_down_files = list.files(output_path, pattern = "Drill_Down_Critical_Habitat.*\\.tif$", full.names=TRUE)
ch_drill_down_file = ch_drill_down_files[which.max(file.info(ch_drill_down_files)$ctime)]
ch2024_drill_down = rast(ch_drill_down_file)
activeCat(ch2024_drill_down) <- "VALUE"

ch2018_drill_down = rast("outputs/paper/ch2018_drill_down.tif")
activeCat(ch2018_drill_down) <- "VALUE"
toc()

df = cats(ch2024_drill_down)[[1]]
df2018 = cats(ch2018_drill_down)[[1]] |>
  select(VALUE,C1:C5) |>
  pivot_longer(C1:C5, names_to = "CRITERIA_NAMES", values_to = "ALL_FEATURES") |>
  separate_longer_delim(ALL_FEATURES,", ") |>
  separate_longer_delim(ALL_FEATURES,"; ") |>
  mutate(across(ALL_FEATURES, ~ na_if(str_trim(.x),"")))

ch2024_drill_down_areas = expanse(ch2024_drill_down,unit="km",byValue=TRUE) |>
  select(-layer) |>
  mutate(value=as.numeric(value))

df = left_join(df,ch2024_drill_down_areas,by=c("VALUE"="value"))

ch2018_drill_down_areas = expanse(ch2018_drill_down,unit="km",byValue=TRUE) |>
  select(-layer) |>
  mutate(value=as.numeric(value))

df2018 = left_join(df2018,ch2018_drill_down_areas,by=c("VALUE"="value"))

cat("mask drill down with CH changes\n")
tic()
ch_addition_mask = mask(ch2024_drill_down,ch_changes,maskvalue=2,inverse=TRUE)
ch_addition_drill_down = expanse(ch_addition_mask,unit="km",byValue=TRUE)[,2:3] |>
  rename(ADDITION=area)

ch_upgrade_mask = mask(ch2024_drill_down,ch_changes,maskvalue=1,inverse=TRUE)
ch_upgrade_drill_down = expanse(ch_upgrade_mask,unit="km",byValue=TRUE)[,2:3] |>
  rename(UPGRADE=area)

ch_downgrade_mask = mask(ch2024_drill_down,ch_changes,maskvalue=-1,inverse=TRUE)
ch_downgrade_drill_down = expanse(ch_downgrade_mask,unit="km",byValue=TRUE)[,2:3] |>
  rename(DOWNGRADE=area)

ch_downgrade_mask_2018 = mask(ch2018_drill_down,ch_changes,maskvalue=-1,inverse=TRUE)
ch_downgrade_drill_down_2018 = expanse(ch_downgrade_mask_2018,unit="km",byValue=TRUE)[,2:3] |>
  rename(DOWNGRADE=area)

ch_removal_mask = mask(ch2018_drill_down,ch_changes,maskvalue=-2,inverse=TRUE)
ch_removal_drill_down = expanse(ch_removal_mask,unit="km",byValue=TRUE)[,2:3] |>
  rename(REMOVAL=area)
toc()

ch_changes_df_2024 = reduce(list(ch_addition_drill_down,
           ch_upgrade_drill_down,
           ch_downgrade_drill_down), full_join, by = "value") |>
  mutate(value=as.numeric(value)) |>
  mutate(across(2:4, ~ replace_na(.x,0)))

ch_changes_df_2018 = reduce(list(ch_downgrade_drill_down_2018,ch_removal_drill_down), full_join, by = "value") |>
  mutate(value=as.numeric(value)) |>
  mutate(across(2:3, ~ replace_na(.x,0)))

df = left_join(df,ch_changes_df_2024,by=c("VALUE"="value"))
df2018 = left_join(df2018,ch_changes_df_2018,by=c("VALUE"="value"))

df_pivot = pivot_longer(df,starts_with(c("L_","P_")),names_to="Feature",values_to="Feature_Present")

cat("create output tables\n")
filter(df_pivot,Feature_Present==1) |>
  group_by(Feature) |>
  summarise(feat_area=sum(area,na.rm=TRUE),
            add_area=sum(ADDITION,na.rm=TRUE)) |>
  mutate(total_add_area=expanse(ch_addition_mask,unit="km")[1,2],
         pct_of_add=add_area/total_add_area,
         pct_of_feat=add_area/feat_area) |>
  arrange(desc(pct_of_add)) |>
  write.csv("outputs/paper/feat_addition_areas.csv", row.names = FALSE)

filter(df_pivot,Feature_Present==1) |>
  group_by(Feature) |>
  summarise(feat_area=sum(area,na.rm=TRUE),
            upgr_area=sum(UPGRADE,na.rm=TRUE)) |>
  mutate(total_upgr_area=expanse(ch_upgrade_mask,unit="km")[1,2],
         pct_of_upgr=upgr_area/total_upgr_area,
         pct_of_feat=upgr_area/feat_area) |>
  arrange(desc(pct_of_upgr)) |>
  write.csv("outputs/paper/feat_upgrade_areas.csv", row.names = FALSE)

filter(df_pivot,Feature_Present==1) |>
  group_by(Feature) |>
  summarise(feat_area=sum(area,na.rm=TRUE),
            downgr_area=sum(DOWNGRADE,na.rm=TRUE)) |>
  mutate(total_downgr_area=expanse(ch_downgrade_mask,unit="km")[1,2],
         pct_of_downgr=downgr_area/total_downgr_area,
         pct_of_feat=downgr_area/feat_area) |>
  arrange(desc(pct_of_downgr)) |>
  write.csv("outputs/paper/feat_2024_downgrade_areas.csv", row.names = FALSE)

group_by(df2018 |> filter(!is.na(ALL_FEATURES)),ALL_FEATURES) |>
  summarise(down_area=sum(DOWNGRADE,na.rm=TRUE),feat_area=sum(area,na.rm=TRUE)) |>
  mutate(total_down_area=expanse(ch_downgrade_mask,unit="km")[1,2],
         pct_of_down=down_area/total_down_area,
         pct_of_feat=down_area/feat_area) |>
  arrange(desc(pct_of_down)) |>
  write.csv("outputs/paper/feat_2018_downgrade_areas.csv", row.names = FALSE)

group_by(df2018 |> filter(!is.na(ALL_FEATURES)),ALL_FEATURES) |>
  summarise(remove_area=sum(REMOVAL,na.rm=TRUE),feat_area=sum(area,na.rm=TRUE)) |>
  mutate(total_remove_area=expanse(ch_removal_mask,unit="km")[1,2],
         pct_of_remove=remove_area/total_remove_area,
         pct_of_feat=remove_area/feat_area) |>
  arrange(desc(pct_of_remove)) |>
  write.csv("outputs/paper/feat_2018_removal_areas.csv", row.names = FALSE)

features_count = filter(df_pivot,Feature_Present==1) |>
     group_by(VALUE) |>
     summarise(count=n())

ch2024_drill_down_stack = mask(ch2024_drill_down,ch2024_binary,maskvalue=0)[[2:3]]
names(ch2024_drill_down_stack) <- c("Potential","Likely")

cat("create nfeature layers\n")
tic()
features_count_stack = classify(ch2024_drill_down_stack,features_count)
toc()

write.csv(df,"outputs/paper/drill_down_change_areas.csv", row.names = FALSE)

writeRaster(features_count_stack[[1]],"outputs/paper/potential_nfeatures.tif",overwrite=TRUE)
writeRaster(features_count_stack[[2]],"outputs/paper/likely_nfeatures.tif",overwrite=TRUE)