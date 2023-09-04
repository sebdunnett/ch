################################################################
#### PREPROCESSING NON-RED LIST DATA (VECTORS) #################
################################################################

# Author: Seb Dunnett
# Created: 20/07/2023

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,tidyverse,terra,tictoc,geos)

data_path = "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/"

output_path = "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

st_save <- function(sf,filename,outpath){
  fnm = paste0(outpath,filename)
  old_fnm = str_replace(fnm,".shp","_old.shp")
  
  if(old_fnm %in% list.files(outpath, full.names=TRUE)){
    file.remove(old_fnm)
  } else{}
  
  if(fnm %in% list.files(outpath, full.names=TRUE)){
    file.rename(fnm,old_fnm)
  } else{}
  
  st_write(sf,fnm)
}

likely_files = list.files(output_path) %>% 
  keep(.,str_detect(.,"^L_") & str_detect(.,".gpkg")) %>% 
  discard(.,str_detect(.,"_old"))

potential_files = list.files(output_path) %>% 
  keep(.,str_detect(.,"^P_") & str_detect(.,".gpkg")) %>% 
  discard(.,str_detect(.,"_old"))

likely_list = lapply(likely_files, FUN=function(x){st_read(paste0(output_path,x))})
potential_list = lapply(potential_files, FUN=function(x){st_read(paste0(output_path,x))})

Likely_Critical_Habitat = do.call(what = sf:::rbind.sf, args = likely_list)
Potential_Critical_Habitat = do.call(what = sf:::rbind.sf, args = potential_list)

likely_polys = filter(Likely_Critical_Habitat, st_geometry_type(Likely_Critical_Habitat)=="MULTIPOLYGON")
likely_pts = filter(Likely_Critical_Habitat, st_geometry_type(Likely_Critical_Habitat)=="MULTIPOINT")
likely_gc = filter(Likely_Critical_Habitat, st_geometry_type(Likely_Critical_Habitat)=="GEOMETRYCOLLECTION")

potential_polys = filter(Potential_Critical_Habitat, st_geometry_type(Potential_Critical_Habitat)=="MULTIPOLYGON")
potential_pts = filter(Potential_Critical_Habitat, st_geometry_type(Potential_Critical_Habitat)=="MULTIPOINT")
potential_gc = filter(Potential_Critical_Habitat, st_geometry_type(Potential_Critical_Habitat)=="GEOMETRYCOLLECTION")

likely_gc_polys = st_collection_extract(likely_gc,"POLYGON") %>% 
  group_by(Feature) %>% summarise(geom=st_combine(geom)) %>% 
  left_join(st_drop_geometry(likely_gc) %>% dplyr::select(Type,Feature,C1:C5), by="Feature")
likely_gc_pts = st_collection_extract(likely_gc,"POINT") %>% 
  group_by(Feature) %>% summarise(geom=st_combine(geom)) %>% 
  left_join(st_drop_geometry(likely_gc) %>% dplyr::select(Type,Feature,C1:C5), by="Feature")

potential_gc_polys = st_collection_extract(potential_gc,"POLYGON") %>% 
  group_by(Feature) %>% summarise(geom=st_combine(geom)) %>% 
  left_join(st_drop_geometry(likely_gc) %>% dplyr::select(Type,Feature,C1:C5), by="Feature")
potential_gc_pts = st_collection_extract(potential_gc,"POINT") %>% 
  group_by(Feature) %>% summarise(geom=st_combine(geom)) %>% 
  left_join(st_drop_geometry(likely_gc) %>% dplyr::select(Type,Feature,C1:C5), by="Feature")

likely_polys = rbind(likely_polys,likely_gc_polys)
likely_pts = rbind(likely_pts,likely_gc_pts)

potential_polys = rbind(potential_polys,potential_gc_polys)
potential_pts = rbind(potential_pts,potential_gc_pts)

cat("saving likely & potential combined vector geopackages...")

st_save(sf=likely_polys, filename="Likely_Critical_Habitat_polys.shp", outpath="C:/Users/sebastiandu/Documents/")
st_save(sf=likely_pts, filename="Likely_Critical_Habitat_pts.shp", outpath="C:/Users/sebastiandu/Documents/")

st_save(sf=potential_polys, filename="Potential_Critical_Habitat_polys.shp", outpath="C:/Users/sebastiandu/Documents/")
st_save(sf=potential_pts, filename="Potential_Critical_Habitat_pts.shp", outpath="C:/Users/sebastiandu/Documents/")

cat("done\n")
