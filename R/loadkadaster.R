#' Retrieve kadaster information from PDOK WFS service
#'
#' Retrieve kadater information for area of interest
#' @param sf Spatial feature object
#' @param expand Extension in meters
#' @param layer Layer of kadastrale kaart, supported are "Perceel", "KadastraleGrens", "Nummeraanduidingreeks", "Bebouwing" and "OpenbareRuimteNaam"
#'
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs st_cast
#' @importFrom data.table setnames
#'
#' @examples
#' kadastrale_percelen <- loadkadaster(sf = aoi, expand = 20, layer = "Perceel")

#'@export
loadkadaster <- function(sf, expand, layer) {
  #set expand to 20 when missing
  if(missing(expand) == TRUE){
    expand <- 20
    print("Expand missing. Set to 20 meter (default).")
  }

  #set layer to perceel when missing
  if(missing(layer) == TRUE){
    layer <- "Perceel"
    print("Type missing. Set to perceel (default).")
  }



  #buffer sf object (transform to amersfoort first)
  sf <- st_transform(sf, crs = st_crs(28992))
  sf_buffered <- st_buffer(sf, expand)

  #wfs capabilities
  wfs_capabilities <-
    "https://service.pdok.nl/kadaster/kadastralekaart/wfs/v5_0?request=GetCapabilities"
  #browseURL(wfs_capabilities)

  #set path and variable of interest
  wfs_path <- "https://service.pdok.nl/kadaster/kadastralekaart/wfs/v5_0?"
  wfs_service <- "SERVICE=WFS"
  wfs_request <- "REQUEST=GetFeature"
  wfs_version <- "VERSION=2.0.0"
  wfs_typename <- paste0("TYPENAMES=", layer)
  wfs_outputformat <- "OutputFormat=application/json"


  #set bbox
  xmin <- st_bbox(sf_buffered)[1] |> round(0)
  xmax <- st_bbox(sf_buffered)[3] |> round(0)
  ymin <- st_bbox(sf_buffered)[2] |> round(0)
  ymax <- st_bbox(sf_buffered)[4] |> round(0)
  wfs_bbox <- paste0("bbox=", xmin, ",", ymin, ",",  xmax, ",", ymax)

  #create path
  wfs <- paste(
    wfs_path,
    wfs_service,
    wfs_request,
    wfs_version,
    wfs_typename,
    wfs_outputformat,
    wfs_bbox,
    sep = "&"
  )

  #read as sf object
  kadaster <- st_read(wfs, quiet = TRUE)

  #return
  return(kadaster)
}
