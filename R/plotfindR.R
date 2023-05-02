#' Finding suitable points within monitoring plots to use for groundwater distance computation
#' @description This function uses a digital elevation model along with polygons representing e.g. vegetation plots to obtain
#' the plot centroid, lowest or heighest point of the plot. The output can then be used to speed up the computation in the
#' ExtractR function.
#' @param dem Path to a digital elevation model stored in any terra::rast() compatible raster format; has to be a single band raster
#' @param loc Path to the polyons representing the observation areas. Can be any sf::st_read() compatible format
#' @param mode Defines the location within the plot area to be returned and can be one of the following:
#' "lowest", "heighest", "centroid"; Default is "Centroid"
#' @return Simple feature object containing the lowest/heighest points or centroids of each polygon of loc


plotfindR<-function(dem, loc, mode="centroid"){

  #Read the data and assign appropriate layer name and ID
  dem<-terra::rast(dem)
  names(dem)<-"DEM"
  locs<-sf::st_read(loc)
  locs$ID<-1:nrow(locs)

  #Get the centroids
  if (mode=="centroid") {
    pts<-sf::st_centroid(locs)
    return(pts)

  }

  #for lowest points
  if (mode=="lowest") {
    pts<-extract(dem,vect(loc),xy=TRUE,exact=TRUE) %>%
      filter(fraction>0.9) %>%
      group_by(ID) %>%
      slice(which.min(DEM)) %>%
      ungroup() %>%
      st_as_sf(coords=c("x","y"),crs=crs(loc))

    pts<-dplyr::inner_join(pts,locs,by="ID") %>%
      select(-fraction)
    return(pts)
  }

  #for heighest points
  if (mode=="heighest") {
    pts<-extract(dem,vect(loc),xy=TRUE,exact=TRUE) %>%
      filter(fraction>0.9) %>%
      group_by(ID) %>%
      slice(which.min(DEM)) %>%
      ungroup() %>%
      st_as_sf(coords=c("x","y"),crs=crs(loc))

    pts<-inner_join(locs,pts,by="ID") %>%
      select(-fraction)
    return(pts)
  }
}
