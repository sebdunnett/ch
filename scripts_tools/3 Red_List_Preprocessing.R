library(pacman)
p_load(tidyverse,sf,terra,tictoc,geos,giscoR,scico,units)

tic("processing RL ranges")

# location of full Red List geodatabase
rl_gdb = "raw_data/IUCN_RL_2022_2_Species_Data.gdb"

output_path = "scratch/"

# import helper functions
source("scripts_tools/spatial_processing_functions.R")

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
rl_fix = fix_sf(rl)

# combine with lookup table
rl_full = left_join(rl_fix,rl_lookup,by="id_no")

rl_sd = 3
rl_areas = units::drop_units(st_area(rl_full))
area_3SD = rl_sd*sd(rl_areas) + mean(rl_areas)

rl_area_trim = rl_full[which(rl_areas<area_3SD),]

# buffer to 50km (sf computes geodesic buffers with lat/long data)
# sf doesn't handle dateline buffering well; terra does

cat("add 50km geodesic buffers\n")

antim_check = st_as_sfc(st_bbox(c(xmin=179, ymin=-90, xmax=180, ymax=90), crs = st_crs(4326))) |>
  rbind(st_as_sfc(st_bbox(c(xmin=-180, ymin=-90, xmax=-179, ymax=90), crs = st_crs(4326)))) |>
  st_as_sfc(crs=4326)

idl = st_intersects(rl_area_trim,antim_check) |>
  map_int(length)

rl_full_buffered = rl_area_trim[!idl>0,] |>
  st_buffer(dist=50000,max_cells=5000)

rl_full_buffered_antim = rl_area_trim[idl>0,]

west = rl_full_buffered_antim |>
  st_buffer(50000,max_cells=5000)|>
  st_crop(st_bbox(c(xmin=-180,xmax=0,ymin=-90,ymax=90),crs=st_crs(4326))) |>
  st_make_valid()

east = rl_full_buffered_antim |>
  st_buffer(50000,max_cells=5000)|>
  st_crop(st_bbox(c(xmin=0,xmax=180,ymin=-90,ymax=90),crs=st_crs(4326))) |>
  st_make_valid()

rl_full_buffered_all = bind_rows(rl_full_buffered,west,east)

# in case any being loaded into Google Earth Engine
# has a limit of 100k vertices per feature
p_load(mapview)
if(any(npts(rl_full_buffered_all, by_feature=TRUE)>10)){
  cat("Google Earth Engine vertices limit per feature is 100k; try sf::st_simplify to reduce feature complexity")
} else{}

# filter final data by IUCN category
L_C1_IUCN_CR_D = filter(rl_full_buffered_all, category=="CR" & str_detect(criteria,"D")) %>%
  st_faster_union() %>%
  mutate(Type="Likely", Feature="CR species under criterion D") %>% 
  filter(st_geometry_type(.)=="MULTIPOLYGON")
L_C1_IUCN_EN_D = filter(rl_full_buffered_all, category=="EN" & str_detect(criteria,"D")) %>% 
  st_faster_union() %>% 
  mutate(Type="Likely", Feature="EN species under criterion D") %>% 
  filter(st_geometry_type(.)=="MULTIPOLYGON")
P_C1_IUCN_VU_D2 = filter(rl_full_buffered_all, category=="VU" & str_detect(criteria,"D2|D1+2")) %>% 
  st_faster_union() %>% 
  mutate(Type="Potential", Feature="VU species under criterion D2") %>% 
  filter(st_geometry_type(.)=="MULTIPOLYGON")

P_C1_IUCN_Great_Apes = filter(rl_full, family == "HOMINIDAE") |>
  st_buffer(dist=50000,max_cells=1000) |>
  fix_sf() |>
  st_faster_union() |> 
  mutate(Type="Potential", Feature="Great Apes species ranges")

# check output folder for previously made files
# rename old files (and delete older)
# save

st_save(sf=L_C1_IUCN_CR_D, filename="L_C1_IUCN_CR_D_polys.shp", outpath=output_path)
st_save(sf=L_C1_IUCN_EN_D, filename="L_C1_IUCN_EN_D_polys.shp", outpath=output_path)
st_save(sf=P_C1_IUCN_VU_D2, filename="P_C1_IUCN_VU_D2_polys.shp", outpath=output_path)
st_save(sf=P_C1_IUCN_Great_Apes, filename="P_C1_IUCN_Great_Apes_polys.shp", outpath=output_path)

# # plotting
# ggplot(gisco_coastallines) +
#   geom_sf(col=NA) +
#   geom_sf(data=rl_full_buffered,aes(fill=category),alpha=0.5,col=NA) +
#   scale_fill_scico_d(palette="batlow", name=NULL) +
#   cowplot::theme_map()

cat("done\n")
toc()

# test = slice_sample(rl_full,n=4)
# 
# test$fct = 1:4
# 
# test_buffs = st_buffer(test,dist=50000,max_cells=5000)
# test_buffs2 = st_transform(test,"ESRI:54032") |>
#   st_buffer(50000) |>
#   st_transform(4326)
# 
# p_load(tmap)
# tm_shape(test) +
#   tm_fill() +
#   tm_shape(test_buffs) +
#   tm_borders(col="red") +
#   tm_shape(test_buffs2) +
#   tm_borders(col="blue") +
#   tm_facets('fct') +
#   tm_grid()
