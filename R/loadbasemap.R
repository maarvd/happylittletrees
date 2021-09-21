#' Retrieve Aerial basemaps
#'
#' Retrieve areal basemaps using the bbox of an sf object
#' @param sf Spatial feature object
#' @param source either "hybrid", "aerial", "topo" or "topo.v2 as a source of the basemap. Defaults to "hybrid".
#' @param expand Extension factor of the bounding box If 1, the bounding box is unchanged. Values smaller than 1 reduces the bounding box,
#' and values larger than 1 enlarges the bounding box. Defaults to 1.2.
#' @param zoom Zoom level (defaults to 18)
#' @examples
#' basemap <- loadbasemap(parcels, "hybrid", 1.2, 18)
#'

#' @export
loadbasemap <- function(sf, source, expand, zoom){
  #set expand to 1.2 when missing
  if(missing(expand) == TRUE){
    expand <- 1.2
  }

  #set source to google when missing
  if(missing(source) == TRUE){
    source <- "google"
  }

  #set zoom to 18 when missing
  if(missing(zoom) == TRUE){
    zoom <- 18
  }

  #load basemap based on the specified source
  if(source == "hybrid"){
      basemap <- tmaptools::read_osm(sf, type = 'https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}', zoom = zoom, ext = expand)
    } else if(source == "topo"){
      basemap <- tmaptools::read_osm(sf, type = 'https://mt1.google.com/vt/lyrs=r&x={x}&y={y}&z={z}', zoom = zoom, ext = expand)
    } else if(source == "topo.v2"){
      basemap <- tmaptools::read_osm(sf, type = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.png', zoom = zoom, ext = expand)
    } else if(source == "aerial"){
      basemap <- tmaptools::read_osm(sf, type = 'https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}', zoom = zoom, ext = expand)
    }

  #return
  return(basemap)
}
