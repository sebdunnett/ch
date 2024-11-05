# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,terra,sf,giscoR,scico,cowplot,kableExtra,scales,vapour,units)
terraOptions(progress=0,memfrac=0.9)

coral_files = list.files("O:/f01_projects_active/Global/p08868_CriticalHabitatUpdate/raw_data/ZSL-001-ModelledOctocorals2012/02_Data_sources/Restricted-GeoTiffHighRes", full.names=TRUE) %>% 
  keep(str_ends(.,".tif") & !str_detect(.,"Consensus"))

cub = gisco_get_countries(country="CUB")

coral = crop(rast(coral_files),cub)

plot(coral>90)
