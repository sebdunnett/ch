library(pacman)
p_load(tidyverse,sf,tictoc,giscoR,scico)

tic("processing RL ranges")

# location of full Red List geodatabase
rl_gdb = "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/IUCN_RL_2022_2_Species_Data.gdb"

output_path = "O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/scratch/"

# helper function to fix invalid geometries 
fix_sf <- function(sf) {
  # Check validity of geometries
  valid = st_is_valid(sf)
  
  # If all geometries are valid, return the original sf object
  if (all(valid)) {
    message("All geometries are valid.")
    return(sf)
  }
  
  # If there are invalid geometries, try to fix them
  fixed_sf = st_make_valid(sf)
  
  # Check validity of the fixed geometries
  valid_fixed = st_is_valid(fixed_sf)
  
  # If all geometries are now valid, return the fixed sf object
  if (all(valid_fixed)) {
    message("All invalid geometries have been fixed.")
    return(fixed_sf)
  }
  
  # If there are still invalid geometries, remove them
  valid_indices = which(valid_fixed)
  invalid_indices = which(!valid_fixed)
  num_removed <- length(invalid_indices)
  message(paste0("Removed ", num_removed, " invalid geometries."))
  return(fixed_sf[valid_indices, ])
}

cat("read in lookup and pre-filter species IDs\n")

# extract data layer names and discard subset layers
rl_gdb_layers = st_layers(rl_gdb)$name %>% 
  discard(str_detect(.,"CR_EN") | str_detect(.,"endemic"))

# specify names for each layer
rl_list = keep(rl_gdb_layers,str_detect(rl_gdb_layers,"List"))
rl_ranges = keep(rl_gdb_layers,str_detect(rl_gdb_layers,"Ranges"))
rl_points = keep(rl_gdb_layers,str_detect(rl_gdb_layers,"Points"))

# read in lookup
tic("read in lookup")
rl_lookup = st_read(rl_gdb,layer=rl_list,quiet=TRUE)
toc()

# filters for critical habitat
# only CR, EN, and VU species
# with criteria D
# pull out id_no for species meeting these requirements
rl_CH_subset = filter(rl_lookup,
                      category %in% c("CR","EN","VU") &
                        str_detect(criteria,"D") &
                        biome_marine == "false" |
                        family == "HOMINIDAE") %>% 
  pull(id_no) %>% 
  as.character

# set up sql query to only read in RL ranges of species
# meeting our requirements
# excludes extinct ranges (presence = 5)
rl_CH_subset = toString(sprintf("%s", rl_CH_subset)) 
sql_fmt = paste0("select * from \"",rl_ranges,"\" where id_no in (%s) and presence not in (2,5)")
sql = sprintf(sql_fmt, rl_CH_subset)

# read in species ranges
# still takes a while
cat("read in RL ranges of interest\n")
tic("read in RL ranges of interest")
rl = st_read(rl_gdb, query=sql, quiet=TRUE)
toc()

# a couple of invalid geometries to fix
rl = fix_sf(rl)

# combine with lookup table
rl_full = left_join(rl,rl_lookup,by="id_no")

# buffer to 50km (project to equal-area to do and back again)
cat("buffer ranges by 50km transforming to and from equal-area\n")
rl_full_buffered = st_transform(rl_full, "ESRI:54009") |>
  st_buffer(50000) |>
  st_transform(4326) |>
  fix_sf()

# in case any being loaded into Google Earth Engine
# has a limit of 100k vertices per feature
p_load(mapview)
if(any(npts(rl_full_buffered, by_feature=TRUE)>10)){
  cat("Google Earth Engine vertices limit per feature is 100k; try sf::st_simplify to reduce feature complexity")
} else{}

# filter final data by IUCN category
L_C1_IUCN_CR_D = filter(rl_full_buffered, category=="CR" & str_detect(criteria,"D")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type="Likely", Feature="CR species under criterion D",
         C1=1,C2=0,C3=0,C4=0,C5=0)
L_C1_IUCN_EN_D = filter(rl_full_buffered, category=="EN" & str_detect(criteria,"D")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type="Likely", Feature="EN species under criterion D",
         C1=1,C2=0,C3=0,C4=0,C5=0)
P_C1_IUCN_VU_D2 = filter(rl_full_buffered, category=="VU" & str_detect(criteria,"D2|D1+2")) %>% 
  st_combine() %>% st_sf() %>% 
  mutate(Type="Potential", Feature="VU species under criterion D2",
         C1=1,C2=0,C3=0,C4=0,C5=0)

great_apes = filter(rl_full, family == "HOMINIDAE")

# check output folder for previously made files
# rename old files (and delete older)
# save
cr_file = paste0(output_path,"L_C1_IUCN_CR_D.gpkg")
en_file = paste0(output_path,"L_C1_IUCN_EN_D.gpkg")
vu_file = paste0(output_path,"P_C1_IUCN_VU_D2.gpkg")

output_files = c(cr_file,en_file,vu_file)
old_files = str_replace(output_files,".gpkg","_old.gpkg")

if(any(old_files %in% list.files(output_path, full.names = TRUE))){
  file.remove(old_files)
} else{}

if(any(output_files %in% list.files(output_path, full.names = TRUE))){
  file.rename(output_files,old_files)
} else{}

cat("saving\n")
st_write(L_C1_IUCN_CR_D,cr_file)
st_write(L_C1_IUCN_EN_D,en_file)
st_write(P_C1_IUCN_VU_D2,vu_file)

## plotting
# ggplot(gisco_coastallines) +
#   geom_sf(col=NA) +
#   geom_sf(data=rl_full_buffered,aes(fill=category),alpha=0.5,col=NA) +
#   scale_fill_scico_d(palette="batlow", name=NULL) +
#   cowplot::theme_map()

cat("done\n")
toc()
