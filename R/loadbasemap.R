#' Retrieve basemaps
#'
#' Retrieve areal basemaps using the bbox of an sf object
#' @param sf Spatial feature object
#' @param source either "hybrid", "aerial", "topo" or "topo.v2 as a source of the basemap. Defaults to "hybrid".
#' @param expand Extension factor of the bounding box If 1, the bounding box is unchanged. Values smaller than 1 reduces the bounding box,
#' and values larger than 1 enlarges the bounding box. Defaults to 1.2.
#' @param zoom Zoom level (defaults to 18)
#' @examples
#' basemap <- loadbasemap(parcels, "hybrid", 1.2, 18)
#'ggplot() + ggspatial::layer_spatial(basemap)

#' @export
loadbasemap <- function(sf, source, expand, zoom){
  #set expand to 1.2 when missing
  if(missing(expand) == TRUE){
    expand <- 1.2
    print("Expand missing. Set to 1.2 (default).")
  }

  #set source to google when missing
  if(missing(source) == TRUE){
    source <- "hybrid"
    print("Source missing. Set to hybrid (default).")
  }

  #set zoom to 18 when missing
  if(missing(zoom) == TRUE){
    sf <- sf::st_transform(sf, 28992) #transform to amersfoort
    bbox <- sf::st_bbox(sf) #extract bbox
    bbox <- rosm::extract_bbox(bbox)
    bbox <- matrix(bbox, ncol = 2, byrow = TRUE)
    zoom <- suppressWarnings(rosm:::tile.raster.autozoom(bbox,
                                epsg= 28992)) + 8 #autozoom
    print(paste0("Autozoom based on bbox. Zoomlevel = ", zoom))
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
