#' Retrieve Aerial basemaps
#'
#' Load (and save to /dev) areal basemaps using the bbox of an sf object
#' @param sf Spatial feature object
#' @param source either "google" or "esri" as a source of the basemap. Defaults to "google".
#' @param expand Extension factor of the bounding box If 1, the bounding box is unchanged. Values smaller than 1 reduces the bounding box, and values larger than 1 enlarges the bounding box. Defaults to 1.2.
#' @examples
#' basemap <- loadbasemap(parcels, "google", 1.2)


loadbasemap <- function(sf, source, expand){
  #set expand to 1.2 when missing
  if(missing(expand) == TRUE){
    expand <- 1.2
  }

  #set source to google when missing
  if(missing(source) == TRUE){
    source <- "google"
  }

  #load basemap based on the specified source
  if(source == "google"){
      basemap <- tmaptools::read_osm(sf, type = 'https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}', zoom = 18, ext = expand)
    } else if(source == "esri"){
      basemap <- stars::read_stars("dev/esri_luchtfoto.tif")
    } else{
      basemap <- tmaptools::read_osm(sf, type = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.png', zoom = 18, ext = expand)
    }

  #return
  return(basemap)
}

