plot_data_sf <- function(data,wrld,bkg,palette="acton",revCol=FALSE){
  if(revCol){
    pal = rev(scico(palette=palette,n=3))
  } else{
    pal = scico(palette=palette,n=3)
  }
  
  if(st_geometry_type(data) %in% c("POINT","MULTIPOINT")){
    ggplot(bkg) +
      geom_sf(col=NA,fill=pal[1]) +
      geom_sf(data=wrld, col=NA, fill=pal[2]) +
      geom_sf(data=data, col=pal[3]) +
      theme_map()
  }
  
  else if(st_geometry_type(data) %in% c("POLYGON","MULTIPOLYGON")){
    ggplot(bkg) +
      geom_sf(col=NA,fill=pal[1]) +
      geom_sf(data=wrld, col=NA, fill=pal[2]) +
      geom_sf(data=data, col=NA, fill=pal[3]) +
      theme_map()
  } else{}
  
}

plot_inset_sf <- function(data,wrld,bkg,palette="acton",revCol=FALSE,inset_x=0.5,inset_y=0.5,inset_width=0.5,inset_height=0.5){
  if(revCol){
    pal = rev(scico(palette=palette,n=3))
  } else{
    pal = scico(palette=palette,n=3)
  }
  
  inset = ggplot(bkg) +
    geom_sf(col=NA,fill=pal[1]) +
    geom_sf(data=wrld, col=NA, fill=pal[2]) +
    geom_sf(data=data,fill=pal[3],col=NA) +
    geom_sf(data=st_as_sfc(st_bbox(data)),col=pal[3],fill=NA) +
    theme_map() +
    theme(panel.background = element_rect(colour="white"))
  
  bkg = st_crop(bkg,data)
  wrld = st_crop(wrld,data)
    
  if(st_geometry_type(data) %in% c("POINT","MULTIPOINT")){
    main = ggplot(bkg) +
      geom_sf(col=NA,fill=pal[1]) +
      geom_sf(data=wrld, col=NA, fill=pal[2]) +
      geom_sf(data=data, col=pal[3]) +
      theme_map()
    
    ggdraw(main) +
      draw_plot(inset,x=inset_x,y=inset_y,width=inset_width,height=inset_height)
    
  }
  
  else if(st_geometry_type(data) %in% c("POLYGON","MULTIPOLYGON")){
    main = ggplot(bkg) +
      geom_sf(col=NA,fill=pal[1]) +
      geom_sf(data=wrld, col=NA, fill=pal[2]) +
      geom_sf(data=data, col=NA, fill=pal[3]) +
      theme_map()
    
    ggdraw(main) +
      draw_plot(inset,x=inset_x,y=inset_y,width=inset_width,height=inset_height)
    
  } else{}
  
}

plot_rst_sensitivity <- function(data_WGS,wrld,bkg,focus_ext,palette="acton",revCol=FALSE){
  
  if(revCol){
    pal = rev(scico(palette=palette,n=3))
    dir=-1
  } else{
    pal = scico(palette=palette,n=3)
    dir=1
  }
  
  inset = ggplot(wrld) +
    geom_sf(col=NA) +
    geom_sf(data=st_centroid(st_as_sfc(st_bbox(rast(ext=focus_ext)))),col="red") +
    theme_map() +
    theme(plot.background=element_rect(fill="white", colour=NA))
  
  rst_agg = crop(data_WGS,focus_ext) |>
    as.data.frame(xy=TRUE) |>
    rename(pct=3) |>
    mutate(`>25%`=pct>0.25,`>50%`=pct>0.5,`>75%`=pct>0.75,`>90%`=pct>0.9) |>
    pivot_longer(4:7, names_to="Threshold",values_to="Presence")
  
  main = ggplot(rst_agg, aes(x=x,y=y,fill=Presence)) +
    geom_tile() +
    facet_wrap(~Threshold) +
    scale_fill_scico_d(palette=palette, direction=dir) +
    theme_void() +
    theme(legend.position="bottom")
  
  ggdraw(main) +
    draw_plot(inset,scale=0.3)
  
}

plot_rst <- function(data,wrld,bkg,palette="acton",revCol=FALSE){
  
  if(revCol){
    pal = rev(scico(palette=palette,n=3))
    dir=-1
  } else{
    pal = scico(palette=palette,n=3)
    dir=1
  }
  
  rst_agg = aggregate(data,fact=10,fun="modal")
  
  rst_rpj = project(rst_agg,raster_eq,method="near") |>
    mask(vect(bkg)) |>
    as.data.frame(xy=TRUE) |>
    rename(filtr=3) |>
    filter(filtr == 1)
  
  ggplot(bkg) +
    geom_sf(col=NA,fill=pal[1]) +
    geom_sf(data=wrld, col=NA, fill=pal[2]) +
    geom_tile(data=rst_rpj, aes(x=x,y=y), fill=pal[3]) +
    theme_map()
  
}

area_calc = function(sf){
  if(st_is_valid(sf)){
    area = st_area(sf) |>
      units::set_units(km2) |>
      as.numeric()
  } else if(all(st_is_valid(st_cast(sf,"POLYGON")))){
    area = st_cast(sf,"POLYGON") |>
      st_area() |>
      sum(na.rm=TRUE) |>
      units::set_units(km2) |>
      as.numeric()
  } else if (all(st_is_valid(st_make_valid(st_cast(sf,"POLYGON"))))){
    area = st_cast(sf,"POLYGON") |>
      st_make_valid() |>
      st_area() |>
      sum(na.rm=TRUE) |>
      units::set_units(km2) |>
      as.numeric()
  } else{
    sf_cast = st_cast(sf,"POLYGON")
    i = st_is_valid(sf_cast)
    iv = sf_cast[!i,]
    v = sf_cast[i,]
    
    area1 = v |>
      st_area() |>
      sum(na.rm=TRUE) |>
      units::set_units(km2) |>
      as.numeric()
    
    sf_use_s2(FALSE)
    area2 = iv |>
      st_area() |>
      sum(na.rm=TRUE) |>
      units::set_units(km2) |>
      as.numeric()
    sf_use_s2(TRUE)
    
    area = area1 + area2
  }
  
  return(area)
  
}
