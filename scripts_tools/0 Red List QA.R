# Install packages (if required)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,tidyverse,units,giscoR,scales,scico)

load("C:/Users/sebastiandu/OneDrive - WCMC/Documents/rl_full.rds")

rl_moll <- st_wrap_dateline(rl_full,options = c("WRAPDATELINE=TRUE","DATELINEOFFSET=90")) %>% 
  st_transform("ESRI:54009")

coast <- gisco_get_coastallines(
  resolution = "20",
  epsg = "4326",
  year = "2016"
)

coast_moll = st_wrap_dateline(coast,options = c("WRAPDATELINE=TRUE","DATELINEOFFSET=90")) %>% 
  st_transform("ESRI:54009")

rl_box = st_drop_geometry(rl_full) %>% 
  mutate(data_update= case_when(publication_yr>2016 ~ "Post-2016",
                      TRUE ~ "2016 and before"))

ggplot(rl_box %>% 
         mutate(biome_marine=case_match(
           biome_marine,
           "true" ~ "Marine",
           "false" ~ "Terrestrial & freshwater"
         ))) +
  geom_boxplot(aes(x=factor(category),y=Area,fill=category)) +
  theme_bw() +
  facet_wrap(~biome_marine + data_update) +
  scale_y_log10(
    "Area [km2]",
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  xlab(NULL) +
  scale_fill_scico_d(palette="bam")

ggplot(coast_moll) + 
  geom_sf(col=NA) +
  geom_sf(data=slice_sample(filter(rl_moll, str_detect(criteria,"D")) %>% 
            mutate(biome_marine=case_match(
              biome_marine,
              "true" ~ "Marine",
              "false" ~ "Terrestrial & freshwater"
            )),n=100),
          aes(fill=category), col=NA, alpha=.5) +
  cowplot::theme_map() +
  facet_wrap(~biome_marine, dir="h") +
  scale_fill_scico_d(name = "Category", palette = "batlow") +
  theme(legend.position = "bottom",
        legend.justification = "center")

st_drop_geometry(rl_full) %>% 
  group_by(seasonal,origin,presence) %>% 
  summarise(count=n())

afr = gisco_get_countries(resolution = "20", country = c("COD","KEN","SSD"))

ggplot(coast) +
  geom_sf(color = "grey80") +
  geom_sf(data = filter(rl_full,str_detect(common_name,"Northern White Rhino")), aes(fill=legend),col=NA) +
  geom_sf(data=afr, col="red",fill=NA) +
  geom_sf_text(data=st_centroid(afr), aes(label=NAME_ENGL), col="white") +
  coord_sf(
    xlim = c(10, 52.6),
    ylim = c(-13.7, 14.4)
  ) +
  cowplot::theme_map() +
  scale_fill_scico_d(name=NULL, palette = "batlow") +
  theme(legend.position = "bottom") +
  ggtitle("Northern White Rhino ranges")

indo = gisco_get_countries(resolution = "20", country = "IDN")

ggplot(coast) +
  geom_sf(color = "grey80") +
  geom_sf(data = filter(rl_full,str_detect(common_name,"Sumatran Rhino")), aes(fill=legend),col=NA, alpha=.5) +
  geom_sf(data=indo, col="red",fill=NA) +
  coord_sf(
    xlim = c(96.4, 120.4),
    ylim = c(-5.1, 8.5)
  ) +
  cowplot::theme_map() +
  scale_fill_scico_d(name=NULL, palette = "batlow") +
  theme(legend.position = "bottom") +
  ggtitle("Sumatran Rhino ranges")

ggplot(coast) +
  geom_sf(color = "grey80") +
  geom_sf(data = filter(rl_full,str_detect(common_name,"Grey Falcon")), aes(fill=legend),col=NA, alpha=.5) +
  coord_sf(
    xlim = c(112.9, 153.6),
    ylim = c(-43.6, -9.2)
  ) +
  cowplot::theme_map() +
  scale_fill_scico_d(name=NULL, palette = "batlow") +
  theme(legend.position = "bottom") +
  ggtitle("Grey Falcon ranges")

ggplot(coast) +
  geom_sf(color = "grey80") +
  geom_sf(data = filter(rl_full,str_detect(common_name,"White-winged Nightjar")), aes(fill=legend),col=NA, alpha=.5) +
  coord_sf(
    xlim = c(-81.4, -35.6),
    ylim = c(-50, 10)
  ) +
  cowplot::theme_map() +
  scale_fill_scico_d(name=NULL, palette = "batlow") +
  theme(legend.position = "bottom") +
  ggtitle("White-winged Nightjar ranges")

ggplot(coast_moll) + 
  geom_sf(col=NA) +
  geom_sf(data=filter(rl_moll, str_detect(criteria,"D") & origin==1 & seasonal==1 & biome_marine=="false"), aes(fill=category), col=NA, alpha=.5) +
  cowplot::theme_map() +
  scale_fill_scico_d(name="Category", palette = "batlow")

##################################################################################
# NATIONAL SCALE DATA #
##################################################################################

rl_eck = st_transform(rl_full,"ESRI:54012")

rtn = lapply(c(100,1000,10000),function(dist){

wrld_buffer = gisco_get_countries() %>% 
  st_wrap_dateline(options=c("WRAPDATELINE=TRUE","DATELINEOFFSET=90")) %>% 
  st_transform("ESRI:54012") %>% 
  st_cast("MULTILINESTRING") %>%
  st_buffer(dist) %>%
  st_union() %>% 
  st_as_sf()

testie = lapply(1:nrow(rl_eck),function(x){
  range_lines = st_cast(rl_eck[x,],"MULTILINESTRING")
  original_length = sum(st_length(range_lines))
  
  ix = st_intersection(range_lines,wrld_buffer)
  new_length = sum(st_length(ix))
  
  prop = new_length/original_length
  return(prop)
})

cat(paste0(dist," done/n"))

return(testie)
})

rl_full = mutate(rl_full,
                 prop100m = unlist(rtn[[1]]),
                 prop1km = unlist(rtn[[2]]),
                 prop10km = unlist(rtn[[3]]))

suspects = filter(rl_full, biome_marine=="false" & prop10km>0.88) %>% 
  arrange(desc(Area))

wrld = gisco_get_countries()

lapply(11:20, function(x){
ggplot(st_crop(wrld,suspects[x,])) +
  geom_sf(fill=NA) +
  geom_sf_text(aes(label=NAME_ENGL)) +
  geom_sf(data=suspects[x,],fill="red",alpha=.5) +
  ggtitle(paste0(suspects[x,"binomial.x"]," - ",suspects[x,"common_name"]))
})

rl_full_simpl = st_transform(bind_rows(L_C1_IUCN_CR_D,
                                       L_C1_IUCN_EN_D,
                                       P_C1_IUCN_VU_D2),
                             "ESRI:54009") %>% 
  st_simplify(dTolerance=1000) %>% 
  st_transform(4326) %>%
  fix_sf()

pal = sample(size=1,scico_palette_names())
pal="lajolla"

plts = lapply(c(3,5,7,10,12,15,17,20,50), function(x){
  plot_df = rl_full_simpl %>% filter(units::drop_units(st_area(.)) < (x*sd(units::drop_units(st_area(.)))) + mean(units::drop_units(st_area(.))))
  area_diff_pct = (sum(units::drop_units(st_area(plot_df)))/sum(units::drop_units(st_area(rl_full_simpl))))*100
  area_diff_pct = signif(area_diff_pct,3)
  row_diff = nrow(rl_full_simpl)-nrow(plot_df)
  ggplot(gisco_coastallines %>% st_transform("ESRI:54012")) +
    geom_sf(col=NA) +
    geom_sf(data=plot_df,aes(fill=category),alpha=0.85,col=NA) +
    scale_fill_manual(values=c("#191900","#5A2F22","#C7504B"), name=NULL) +
    cowplot::theme_map() +
    labs(title=paste0("SD: ",x),
         subtitle = paste0(area_diff_pct,"% of total area; ",row_diff," rows missing of ",nrow(rl_full_simpl))) +
    theme(legend.position="none")
})

cowplot::plot_grid(plotlist=plts,nrow=3,ncol=3)
