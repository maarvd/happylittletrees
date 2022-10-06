#' Retrieve Alterra soil map
#'
#' Retrieve 1:50.000 soil map for polygon of interest
#' @param sf Spatial feature polygon
#' @param expand Extension in meters
#' @param mask Boolian stating if desiring to mask the soil map to area of interest
#'
#'
#' @examples
#' bd50 <- loadbd50(sf = parcel, expand = 20, mask = TRUE)


#'@export
loadbd50 <- function(sf, expand, mask){
  #retrieve user info, set folder of bd50
  user <- Sys.getenv()[["USERNAME"]]
  bd50.folder <- paste0("C:/Users/", user, "/SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI/Sven Verweij - NMI-DATA/bodem/alterra/Bodemkaart50/products")

  #read bd50, set crs to amersfoort
  bd50 <- st_read(paste0(bd50.folder, "/bodemkaart50.gpkg"), quiet = TRUE)
  suppressWarnings(st_crs(bd50) <- st_crs(28992))

  #transform sf to amersfoort as well
  sf.transformed <- sf %>% st_transform(28992)

  #expand with x meters
  sf.buffered <- st_buffer(x = sf.transformed, dist = expand)

  #retrieve bbox
  sf.bbox <- st_bbox(sf.buffered)

  #crop to area of interest
  bd50.cropped <- st_crop(bd50, sf.bbox)

  #mask if applicable, else return
  if(mask == FALSE){
    return(bd50.cropped)
  } else if(mask == TRUE){
    bd50.masked <- st_intersection(sf.transformed, bd50.cropped)
    return(bd50.masked)
  }
}
