################################################################
#### REGIONAL PATTERNS ANALYSIS ################################
################################################################

# Author: Seb Dunnett
# Created: 16/07/2025

# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,tidyverse,terra,scico,cowplot)

ipbes_wrld = st_read("raw_data/EEZv8_WVS_DIS_V3_ALL_final_v7disIPBES")

changes = rast("outputs/paper/changes_2018_2024.tif")

changes_binary = segregate(changes)
changes_binary = c(changes_binary,rast(res=1/120,vals=1))
names(changes_binary) <- c("removed","downgraded","nochange","upgraded","added","total")

region_stats = zonal(changes_binary,vect(ipbes_wrld),fun="sum")
region_stats = mutate(region_stats,ipbes_sub=ipbes_wrld$IPBES_sub,.before=removed)

perCat = region_stats |>
  mutate(across(removed:added, ~ .x/sum(.x)))

perCountry = region_stats |>
  mutate(across(removed:added, ~ .x/total))

write.csv(dplyr::select(perCat,-total),"outputs/paper/regional_perCat.csv",row.names=FALSE)
write.csv(dplyr::select(perCountry,-total),"outputs/paper/regional_perCountry.csv",row.names=FALSE)

sf_use_s2(FALSE)
plot_wrld = st_crop(ipbes_wrld,st_bbox(gisco_get_countries()))
sf_use_s2(TRUE)

plot_wrld = st_transform(plot_wrld,"EPSG:8857") |>
  st_simplify(dTolerance=1000)

wrld_perCat = cbind(plot_wrld,perCat[,-1])
wrld_perCountry = cbind(plot_wrld,perCountry[,-1])

a = ggplot(plot_wrld) +
  geom_sf(aes(fill=IPBES_regi,alpha=type),col=NA) +
  geom_sf(data=filter(plot_wrld,type=="Land"),fill=NA) +
  scale_fill_manual(name=NULL,values=c("lightblue","#D85C71","#E0E15A","#7CDF9A","#AC41E2","gray")) +
  scale_alpha_manual(values=c(.6,.6,1),guide="none") +
  cowplot::theme_map() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, 'cm'),
        legend.key.spacing.x = unit(0.1,"lines"),
        legend.key.spacing.y = unit(0.1,"lines"),
        legend.title.position = "top",
        legend.title = element_text(size=7,hjust=0.5),
        legend.text = element_text(size=6, margin=margin(l=0,r=0)),
        legend.justification.bottom = "center",
        legend.box.spacing = unit(0,"lines"))

b = ggplot(wrld_perCountry, aes(fill=removed)) +
  geom_sf(col=NA) +
  scale_fill_scico(name=NULL, palette="acton", labels = scales::label_percent(accuracy=1)) +
  theme_void() +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(0.5, "lines"),
    legend.text.position = "left"
    )))

c = ggplot(wrld_perCountry, aes(fill=downgraded)) +
  geom_sf(col=NA) +
  scale_fill_scico(name=NULL, palette="acton", labels = scales::label_percent(accuracy=.1)) +
  theme_void() +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(0.5, "lines"),
    legend.text.position = "left"
  )))

d = ggplot(wrld_perCountry, aes(fill=nochange)) +
  geom_sf(col=NA) +
  scale_fill_scico(name=NULL, palette="acton", labels = scales::label_percent(accuracy=1)) +
  theme_void() +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(0.5, "lines"),
    legend.text.position = "left"
  )))

e = ggplot(wrld_perCountry, aes(fill=upgraded)) +
  geom_sf(col=NA) +
  scale_fill_scico(name=NULL, palette="acton", labels = scales::label_percent(accuracy=1)) +
  theme_void() +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(0.5, "lines"),
    legend.text.position = "left"
  )))

f = ggplot(wrld_perCountry, aes(fill=added)) +
  geom_sf(col=NA) +
  scale_fill_scico(name=NULL, palette="acton", labels = scales::label_percent(accuracy=1)) +
  theme_void() +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(0.5, "lines"),
    legend.text.position = "left"
  )))

g = plot_grid(a,b,c,d,e,f,ncol=2,labels=letters[1:6])
g