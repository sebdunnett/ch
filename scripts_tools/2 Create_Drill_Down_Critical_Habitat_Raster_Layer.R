#########################################################
#### CREATE DRILL DOWN CRITICAL HABITAT RASTER LAYER ####
#########################################################

# Author: Seb Dunnett
# Created: 10/06/2026

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,terra,tidyverse,units,tictoc,foreign)

# import helper functions
source("scripts_tools/0 spatial_processing_functions.R")

scratch_path = "scratch/"

# path to where you want the output saved
output_path = "outputs/"

lookup = read.csv("scripts_tools/lookup.csv") |>
  mutate(Type_Feature=paste0(Type,"; ",Feature))

raster_WGS = rast(res=1/120)

dir.create(paste0(scratch_path,"raw_tiles"))
dir.create(paste0(scratch_path,"reclassified_tiles"))

likely_rfiles = list.files(scratch_path, full.names=TRUE, pattern="^L_.*\\.tif$") %>% 
  discard(.,str_detect(.,"_uncertainty|_old"))
likely = rast(lapply(likely_rfiles,rast))
names(likely) = paste0("Likely; ",names(likely))
names(likely) = lookup$Short[match(names(likely), lookup$Type_Feature)]

potential_rfiles = list.files(scratch_path, full.names=TRUE, pattern="^P_.*\\.tif$") %>% 
  discard(.,str_detect(.,"_uncertainty|_old"))
potential = rast(lapply(potential_rfiles,rast))
names(potential) = paste0("Potential; ",names(potential))
names(potential) = lookup$Short[match(names(potential), lookup$Type_Feature)]

feature_stack = c(likely,potential)
feature_stack = feature_stack[[order(names(feature_stack))]]

unique_combos = unique(feature_stack) |>
  unite("key", everything(), sep = "_", remove = FALSE) %>% 
  mutate(VALUE=1:nrow(.))

file.remove(list.files(paste0(scratch_path,"raw_tiles")))
file.remove(list.files(paste0(scratch_path,"reclassified_tiles")))

tiles = makeTiles(feature_stack,rast(res=45),filename=paste0(scratch_path,"raw_tiles/tile_.tif"))

for(tile_path in tiles){
  cat(paste0(tile_path,"\n"))
  tile = rast(tile_path)
  reclass.df = as.data.frame(tile, xy=TRUE) |>
    unite("key", -c(x, y), sep = "_", remove = TRUE) |>
    left_join(select(unique_combos,key,VALUE),by="key") |>
    select(x,y,VALUE)
  out = rast(reclass.df, type="xyz", crs=crs(tile))
  writeRaster(out, gsub("raw","reclassified", tile_path), datatype="INT4U")
}

# mosaic reclassified tiles
out_ch = merge(sprc(lapply(list.files(paste0(scratch_path,"reclassified_tiles"),full.names=TRUE), rast)))

out_rat = select(unique_combos,-key)

for(i in 1:5){
  col_name = paste0("C", i)
  cols = lookup$Short[lookup[[col_name]] == 1]
  cols = intersect(cols, names(out_rat))
  out_rat[[col_name]] = as.integer(rowSums(out_rat[, cols, drop = FALSE]) > 0)
}

likely_cols = intersect(lookup$Short[lookup$Type == "Likely"], names(out_rat))
potential_cols = intersect(lookup$Short[lookup$Type == "Potential"], names(out_rat))

out_rat$CH = case_when(
  rowSums(out_rat[, likely_cols, drop = FALSE]) > 0 ~ 10,
  rowSums(out_rat[, potential_cols, drop = FALSE]) > 0 ~ 1,
  .default = 0
)

cell_counts = freq(out_ch)

out_rat = relocate(out_rat,C1:C5, .after = VALUE) |>
  relocate(CH, .after = VALUE) |>
  mutate(COUNT = cell_counts$count, .after = VALUE)

# saving files and removing previous versions
cat("Saving raster...\n")

rast_save(rst=out_ch,filename=paste0("Drill_Down_Critical_Habitat",format(Sys.time(), "_%d%m%Y"),".tif"),outpath=output_path,nms="Drill_Down_Critical_Habitat",dt="INT4U")

cat("Saving RAT...\n")

dbf_file = paste0(output_path,"Drill_Down_Critical_Habitat",format(Sys.time(), "_%d%m%Y"),".tif.vat.dbf")
old_fnm = str_replace(dbf_file,".tif.vat.dbf","_old.tif.vat.dbf")

if(old_fnm %in% list.files(output_path, full.names=TRUE)){
  file.remove(old_fnm)
} else{}

if(dbf_file %in% list.files(output_path, full.names=TRUE)){
  file.rename(dbf_file,old_fnm)
} else{}

foreign::write.dbf(as.data.frame(out_rat), file = dbf_file)