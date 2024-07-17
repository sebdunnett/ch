fix_sf <- function(sf) {
  # Check validity of geometries
  valid = st_is_valid(sf)
  
  # If all geometries are valid, return the original sf object
  if (all(valid)) {
    message("All geometries are valid.")
    return(sf)
  }
  
  idx = which(st_is_valid(sf))
  valid_sf = sf[idx,]
  invalid_sf = sf[-idx,]
  
  # Try to fix them
  # First check for features crossing antimeridian as they're rarely fixed properly
  
  antim_check = st_as_sfc(st_bbox(c(xmin=179, ymin=-90, xmax=180, ymax=90), crs = st_crs(4326))) |>
    rbind(st_as_sfc(st_bbox(c(xmin=-180, ymin=-90, xmax=-179, ymax=90), crs = st_crs(4326)))) |>
    st_as_sfc(crs=4326)
  
  # spherical off for ease (otherwise might not compute)
  sf_use_s2(FALSE)
  idl = st_intersects(invalid_sf,antim_check) %>% map_int(length)
  idl = idl>0
  sf_use_s2(TRUE)
  
  if(any(idl)){
    message("Some invalid geometries cross dateline: fixing those first.")
    idl_sf = invalid_sf[idl,]
    
    dTols = c(10,100,500,1000)
    dTol_ix <- 1
    iv_loop = TRUE
    original_crs = st_crs(idl_sf)
    
    while(any(iv_loop,dTol_ix<length(dTols))){
      dTol = dTols[dTol_ix]
      cat(paste0("dTolerance: ",dTol,"\n"))
      idl_sf_rpj = st_wrap_dateline(idl_sf) %>%
        st_transform("ESRI:54012") %>% 
        st_simplify(dTolerance=dTol) %>% 
        st_transform(original_crs)
      dTol_ix = dTol_ix + 1
      iv_loop = any(!st_is_valid(idl_sf_rpj))
    }
    
    fixed_sf = st_make_valid(invalid_sf[!idl,]) %>% bind_rows(idl_sf_rpj,valid_sf)
    
  } else if(all(!idl)){
    message("No geometries cross dateline.")
    fixed_sf = st_make_valid(invalid_sf) %>% bind_rows(valid_sf)
  }
  
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
  num_invalid <- length(invalid_indices)
  message(paste0(num_invalid, " geometries still invalid. Attempting st_simplify."))
  
  dTols = c(10,100,500,1000)
  dTol_ix <- 1
  iv_loop = TRUE
  original_crs = st_crs(fixed_sf)
  vsf = fixed_sf[valid_indices, ]
  ivsf = fixed_sf[invalid_indices, ]
  
  while(any(iv_loop,dTol_ix<length(dTols))){
    dTol = dTols[dTol_ix]
    cat(paste0("dTolerance: ",dTol,"\n"))
    ivsf_rpj = st_transform(ivsf,"ESRI:54012") %>% 
      st_simplify(dTolerance=dTol) %>% 
      st_transform(original_crs)
    dTol_ix = dTol_ix + 1
    iv_loop = any(!st_is_valid(ivsf_rpj))
  }
  
  if(all(st_is_valid(ivsf_rpj))){
    message("All invalid geometries have been fixed.")
  } else{
    message("Returning with invalid geometries remaining.")
  }
  
  rtn_sf = rbind(vsf,ivsf_rpj)
  
  return(rtn_sf)
}

st_faster_union <- function(sf) {
  
  p_load(mapview)
  
  if(nrow(sf)==0){
    
    message("sf object has 0 records")
    
    return(sf)
    
  } else{}
  
  if(all(st_geometry_type(sf) %in% c("POINT","MULTIPOINT"))){
    
    rtn_sf = st_union(sf) |>
      st_sf()
    
    return(rtn_sf)
    
  } else if(all(st_geometry_type(sf) %in% c("POLYGON","MULTIPOLYGON"))){
    
    if(npts(sf)<10000000){
      
      rtn_sf = st_union(sf) %>% 
        st_sf()
      
      return(rtn_sf)
      
    } else{
      
      rtn_sf = sf |>
        st_wrap_dateline() |>
        st_transform("ESRI:54009") |>
        as_geos_geometry() |>
        geos_make_collection()|>
        geos_make_valid() |>
        geos_unary_union() |>
        geos_make_valid() |>
        st_as_sf() |>
        st_transform(4326)
        
        return(rtn_sf)
      
      }
    
  } else{}
  
}

st_save <- function(sf,filename,outpath){
  
  if(nrow(sf)==0){
    return()
  } else{}
  
  if(tail(str_split_1(filename, "\\."),n=1)=="shp"){
    fnm = paste0(outpath,filename)
    fnm_list = c(fnm,
                 str_replace(fnm,".shp",c(".shx",".prj",".dbf")))
    old_fnm = str_replace(fnm,".shp",c("_old.shp","_old.shx","_old.prj","_old.dbf"))
    if(any(old_fnm %in% list.files(outpath, full.names=TRUE))){
      file.remove(old_fnm)
      } else{}
    if(any(fnm_list %in% list.files(outpath, full.names=TRUE))){
      file.rename(fnm_list,old_fnm)
    } else{}
    
    st_write(sf,fnm,quiet=TRUE)
  
  } else if(tail(str_split_1(filename, "\\."),n=1)=="gpkg"){
    fnm = paste0(outpath,filename)
    old_fnm = str_replace(fnm,".gpkg","_old.gpkg")
    if(old_fnm %in% list.files(outpath, full.names=TRUE)){
      file.remove(old_fnm)
    } else{}
    if(fnm %in% list.files(outpath, full.names=TRUE)){
      file.rename(fnm,old_fnm)
    } else{}
    
    st_write(sf,fnm,quiet=TRUE)
    
  } else{
    message("Must save file as either shapefile or geopackage.")
  }
}

rast_save <- function(rst,filename,outpath,nms,dt){
  fnm = paste0(outpath,filename)
  old_fnm = str_replace(fnm,".tif","_old.tif")

  if(old_fnm %in% list.files(outpath, full.names=TRUE)){
    file.remove(old_fnm)
  } else{}

  if(fnm %in% list.files(outpath, full.names=TRUE)){
    file.rename(fnm,old_fnm)
  } else{}

  writeRaster(x=rst, filename=fnm, datatype=dt, names=nms)
}

st_buffer_antimeridian <- function(sf,dist,max_cells){
  antim_check = st_as_sfc(st_bbox(c(xmin=179, ymin=-90, xmax=180, ymax=90), crs = st_crs(4326))) |>
    rbind(st_as_sfc(st_bbox(c(xmin=-180, ymin=-90, xmax=-179, ymax=90), crs = st_crs(4326)))) |>
    st_as_sfc(crs=4326)
  
  idl = st_intersects(sf,antim_check) |>
    map_int(length)
  
  if(sum(idl==0)){
    message("Data not within 1 degree of antimeridian, using st_buffer")
    
    sf_buff = st_buffer(sf,dist=dist,max_cells=max_cells)
    
    return(sf_buff)
    
  } else{
    if(length(dist>1)){
      dist1 = dist[!idl>0]
      dist2 = dist[idl>0]
      } else{
        dist1 = dist
        dist2 = dist
      }
    
    message("Some data within 1 degree of antimeridian, using modified st_buffer")
    
    buffered = sf[!idl>0,] |>
      st_buffer(dist=dist1,max_cells=max_cells)
    
    buffered_antim = sf[idl>0,]
    
    west = buffered_antim |>
      st_buffer(dist2,max_cells=max_cells)|>
      st_crop(st_bbox(c(xmin=-180,xmax=0,ymin=-90,ymax=90),crs=st_crs(4326))) |>
      st_make_valid()
    east = buffered_antim |>
      st_buffer(dist2,max_cells=max_cells)|>
      st_crop(st_bbox(c(xmin=0,xmax=180,ymin=-90,ymax=90),crs=st_crs(4326))) |>
      st_make_valid()
    
    sf_buff = bind_rows(buffered,west,east)
    
    return(sf_buff)
    
  }
}

