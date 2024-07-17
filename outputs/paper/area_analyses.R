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

abnj = filter(wrld_eez, type=="ABNJ")
eez = filter(wrld_eez, type=="EEZ")
land = filter(wrld_eez, type=="Land")

tic("zonal stats")
ch2023_abnj_areas = zonal(ch2023_binary,vect(abnj),fun="sum")
ch2023_eez_areas = zonal(ch2023_binary,vect(eez),fun="sum")
ch2023_land_areas = zonal(ch2023_binary,vect(land),fun="sum")

ch2018_abnj_areas = zonal(ch2018_binary,vect(abnj),fun="sum")
ch2018_eez_areas = zonal(ch2018_binary,vect(eez),fun="sum")
ch2018_land_areas = zonal(ch2018_binary,vect(land),fun="sum")

ch_changes_abnj_areas = zonal(ch_changes_binary,vect(abnj),fun="sum")
ch_changes_eez_areas = zonal(ch_changes_binary,vect(eez),fun="sum")
ch_changes_land_areas = zonal(ch_changes_binary,vect(land),fun="sum")
toc()

abnj_areas = cbind(abnj,ch2023_abnj_areas,ch2018_abnj_areas,ch_changes_abnj_areas)
eez_areas = cbind(eez,ch2023_eez_areas,ch2018_eez_areas,ch_changes_eez_areas)
land_areas = cbind(land,ch2023_land_areas,ch2018_land_areas,ch_changes_land_areas)

wrld_eez_total_areas = bind_rows(abnj_areas,eez_areas,land_areas)

wrld_eez_total_areas = cbind(wrld_eez_total_areas,zonal(cell_areas,vect(wrld_eez_total_areas),fun="sum"))

wrld_eez_total_areas = wrld_eez_total_areas |>
  mutate(across(Unclassified2023:Addition, ~ .x / area))

write.csv(st_drop_geometry(wrld_eez_total_areas),"O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/outputs/paper/wrld_ch_areas.csv", row.names = FALSE)

ch2023_drill_down = rast("outputs/Critical_Habitat_Drill_Down_WGS.tif")
activeCat(ch2023_drill_down) <- "VALUE"

ch_addition_mask = mask(ch2023_drill_down,ch_addition,maskvalue=0)
ch_addition_vals = unique(values(ch_addition_mask,na.rm=TRUE))

ch_upgrade_mask = mask(ch2023_drill_down,ch_upgrade,maskvalue=0)
ch_upgrade_vals = unique(values(ch_upgrade_mask,na.rm=TRUE))

ch_downgrade_mask = mask(ch2023_drill_down,ch_downgrade,maskvalue=0)
ch_downgrade_vals = unique(values(ch_downgrade_mask,na.rm=TRUE))

# ch_removal_mask = mask(ch2023_drill_down,ch_removal,maskvalue=0)
# ch_removal_vals = unique(values(ch_removal_mask,na.rm=TRUE))

df = cats(ch2023_drill_down)[[1]]

ch_add_total_area = expanse(ch_addition,unit="km",byValue=TRUE)[2,3]
ch_upg_total_area = expanse(ch_upgrade,unit="km",byValue=TRUE)[2,3]
ch_dwg_total_area = expanse(ch_downgrade,unit="km",byValue=TRUE)[2,3]
ch_rmv_total_area = expanse(ch_removal,unit="km",byValue=TRUE)[2,3]

df_pivot = pivot_longer(df,starts_with(c("L_","P_")),names_to="Feature",values_to="Feature_Present")

add_df = filter(df_pivot,VALUE %in% ch_addition_vals & Feature_Present==1)
areas = expanse(mask(ch2023_drill_down,ch_addition,maskvalues=0),unit="km",byValue=TRUE) |>
  dplyr::select(-layer) |>
  mutate(value=as.integer(value))
add_df = left_join(add_df,areas,by=c("VALUE"="value"))
add_df %>% group_by(Feature) %>% summarise(total_area=sum(area)) %>% mutate(percentage=total_area/ch_add_total_area) %>% arrange(desc(percentage))

df_pivot = pivot_longer(df,starts_with("L_"),names_to="Feature",values_to="Feature_Present")

upg_df = filter(df_pivot,VALUE %in% ch_upgrade_vals & Feature_Present==1)
areas = expanse(mask(ch2023_drill_down,ch_upgrade,maskvalues=0),unit="km",byValue=TRUE) |>
  dplyr::select(-layer) |>
  mutate(value=as.integer(value))
upg_df = left_join(upg_df,areas,by=c("VALUE"="value"))
upg_df %>% group_by(Feature) %>% summarise(total_area=sum(area)) %>% mutate(percentage=total_area/ch_upg_total_area) %>% arrange(desc(percentage))

test %>% group_by(VALUE) %>% summarise(no_features=n(),area=mean(area),feature=paste(sort(Feature),collapse=";")) %>% arrange(desc(no_features))

africa = gisco_get_countries(region="Africa") |> st_union()

stack = c(ch2023_drill_down,cellSize(ch2023_drill_down,unit="km"))

shp = gisco_get_countries(country="FRA")

test = extract(stack,vect(shp),ID=FALSE)

testie = group_by(test,VALUE) |>
  summarise(area=sum(area)) |>
  mutate(VALUE=as.integer(VALUE))

shp_df = left_join(testie,df,by="VALUE")

shp_area = sum(shp_df$area)

shp_df_longer = pivot_longer(shp_df,starts_with(c("L_","P_")),names_to="Feature",values_to="Feature_Present") |>
  filter(Feature_Present==1)

shp_summary = group_by(shp_df_longer,VALUE) |>
  summarise(Feature=paste(Feature,collapse=";"),area=mean(area)) |>
  mutate(percentage=area/sum(area)) |>
  slice_max(percentage,n=5)