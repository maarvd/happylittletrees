#' Retrieve Aerial basemaps
#'
#' Load (and save to /dev) areal basemaps using the bbox of an sf object
#' @param sf Spatial feature object
#' @param source either "google" or "esri" as a source of the basemap
#' @param expand value > 1 expands the basemap
#' @examples
#' basemap <- loadbasemap(parcels, "google", 1.2)


loadbasemap <- function(sf, source, expand){
  if(source == "google"){
    if(file.exists("dev/google_luchtfoto.tif")){
      basemap <- stars::read_stars("dev/google_luchtfoto.tif")
    } else{
      basemap <- tmaptools::read_osm(sf, type = 'https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}', zoom = 18, ext = expand)
      stars::write_stars(basemap, dsn = "dev/google_luchtfoto.tif")
    }
  } else if(source == "esri"){
    if(file.exists("dev/esri_luchtfoto.tif")){
      basemap <- stars::read_stars("dev/esri_luchtfoto.tif")
    } else{
      basemap <- tmaptools::read_osm(sf, type = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.png', zoom = 18, ext = expand)
      stars::write_stars(basemap, dsn = "dev/esri_luchtfoto.tif")
    }
  }
  return(basemap)
}

#test if git works
