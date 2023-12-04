######################################################################
#### CALCULATE RASTER AREA UPLIFT & UNCERTAINTIES ####################
######################################################################

# Author: Seb Dunnett
# Created: 13/10/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,terra,tidyverse,units,foreign)

scratch_path = "scratch/"
output_path = "outputs/"

lookup = read.csv("scripts_tools/lookup.csv") %>% 
  arrange(Type,Feature)

feature_lookup = pull(lookup,Short,Feature)

likely_polys_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^L_") & str_detect(.,"_polys.shp"))

potential_polys_files = list.files(scratch_path) %>% 
  keep(.,str_detect(.,"^P_") & str_detect(.,"_polys.shp"))

polys_files = c(likely_polys_files,potential_polys_files)

polys_areas = plyr::ldply(1:length(polys_files),function(x){
  cat(paste0(x,": ",polys_files[x],"\n"))
  sf = st_read(paste0(scratch_path,polys_files[x]),quiet=TRUE)
  
  if(st_is_valid(sf)){
    area = st_area(sf) |>
      units::set_units(km2) |>
      as.numeric()
  } else{
    area = st_cast(sf,"POLYGON") |>
      st_make_valid() |>
      st_area() |>
      sum(na.rm=TRUE) |>
      units::set_units(km2) |>
      as.numeric()
  }
  
  feature = sf$Feature
  
  return(data.frame(Feature=feature,Area_polys_sqkm=area))
  
})

chr = rast("outputs/Critical_Habitat_Drill_Down_WGS.tif")
activeCat(chr) <- "VALUE"

rast_areas_lookup = expanse(chr,byValue=TRUE,unit="km") |>
  dplyr::select(-layer) |>
  mutate(value=as.numeric(value))

ch_df = foreign::read.dbf("outputs/Critical_Habitat_Drill_Down_WGS.tif.vat.dbf")

rast_areas = plyr::ldply(polys_areas$Feature,function(x){
  var = feature_lookup[x]
  vals = filter(ch_df, !!sym(var) == 1) |> pull(VALUE)
  area = filter(rast_areas_lookup, value %in% vals) |> pull(area) |> sum()
  return(data.frame(Feature=x,Area_rast_sqkm=area))
})

area_out = full_join(polys_areas,rast_areas,by="Feature") |>
  mutate(Increase_pct=((Area_rast_sqkm-Area_polys_sqkm)/Area_polys_sqkm)*100)

write.csv(area_out,paste0(output_path,"area_comparisons.csv"),row.names=FALSE)

r = rast(res=1/120)
r1 = crop(r,ext(c(-180,0,0,90)))
r2 = crop(r,ext(c(0,180,0,90)))
r3 = crop(r,ext(c(-180,0,-90,0)))
r4 = crop(r,ext(c(0,180,-90,0)))

lapply(1:length(polys_files),function(x){
  cat(paste0(x,": ",polys_files[x],"\n"))
  sf = st_read(paste0(scratch_path,polys_files[x]),quiet=TRUE)
  
  rr1 = rasterize(sf,r1,cover=TRUE)
  rr2 = rasterize(sf,r2,cover=TRUE)
  rr3 = rasterize(sf,r3,cover=TRUE)
  rr4 = rasterize(sf,r4,cover=TRUE)
  
  writeRaster(merge(rr1,rr2,rr3,rr4),paste0(scratch_path,str_replace(polys_files[x], "\\.shp$", ""),"_uncertainty.tif"))
})
