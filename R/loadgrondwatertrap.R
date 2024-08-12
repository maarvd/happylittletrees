#' Retrieve grondwatertrappenkaart from bodemdata
#'
#' Retrieve grondwatertrappenkaart using the bbox of an sf object
#' @param sf Spatial feature object
#' @param expand Extension in meters. Defaults to 20 meters.
#' @param zoom Zoom level. Defaults to 13
#'
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs
#' @importFrom terra rast crs
#' @importFrom httr GET write_disk
#'
#' @examples
#' basemap <- loadgrondwatertrap(sf = parcels, expand = 20, zoom = 16)

#' @export
loadgrondwatertrap <- function(sf, year, res, expand, zoom){
  #set expand to 20 when missing
  if(missing(expand) == TRUE){
    expand <- 20
    print("Expand missing. Set to 20 meter (default).")
  }

  #set name
  wms_name <- "bodem:gt-mediane-gxg"

  #transform & buffer sf
  sf <- st_transform(sf, crs = st_crs(28992))
  sf_buffered <- st_buffer(sf, expand)

  #set zoom when missing
  if(missing(zoom) == TRUE){
    zoom <- 13
  }

  #capabilities
  wms_capabilities <- "https://bodemdata.nl/geoserver/ows?service=WMS&version=1.3.0&request=GetCapabilities"
  #browseURL(wms_capabilities)

  #base url of wms service
  wms_baseurl <- "https://bodemdata.nl/geoserver/ows?service=WMS&version=1.3.0"

  #settings
  wms_request <- "request=GetMap"
  wms_format <- "format=image/jpeg"
  wms_layers <- paste0("layers=", wms_name)
  wms_epsg <- "crs=EPSG:28992"
  wms_espg_bbox <- "BoundingBox CRS=EPSG:28992"
  wms_version <- "version=1.3.0"

  #set bounding box
  xmin <- st_bbox(sf_buffered)[1] |> round(0)
  xmax <- st_bbox(sf_buffered)[3] |> round(0)
  ymin <- st_bbox(sf_buffered)[2]|> round(0)
  ymax <- st_bbox(sf_buffered)[4]|> round(0)
  wms_bbox <- paste0("bbox=", xmin, ",", ymin, ",",  xmax, ",", ymax)

  #set height and width according to bbox
  wms_width <- (xmax-xmin) * zoom
  wms_height <- (ymax - ymin) * zoom

  #if >2500, adjust
  if(wms_width > wms_height){
    if(wms_width > 2500){
      factor <- 2500 / wms_width
      wms_width <- wms_width * factor
      wms_height <- wms_height * factor
    }
  }else{
    if(wms_height > 2500){
      factor <- 2500 / wms_height
      wms_width <- wms_width * factor
      wms_height <- wms_height * factor
    }
  }

  wms_width <- paste0("width=", wms_width)
  wms_height <- paste0("height=", wms_height)

  #adjust url accordingly
  wms_url <- paste(wms_baseurl,
                   wms_request,
                   wms_format,
                   wms_layers,
                   wms_epsg,
                   wms_version,
                   wms_espg_bbox,
                   wms_bbox,
                   wms_width,
                   wms_height,
                   sep = "&")

  #remove spaces
  wms_url <- gsub(" ", "%", wms_url)

  #download a temporary file to dev
  #create /dev if not existing
  if(!dir.exists("dev")){
    dir.create("dev")
  }

  #if basemap_temporary already exists, create new
  httr::GET(wms_url, write_disk(paste0("dev/grondwatertrap.tiff")))  |> suppressMessages() |> invisible()

  #read
  gt <- rast(paste0("dev/grondwatertrap.tiff")) |> suppressWarnings() #extent will be set

  #read extent
  ext(gt) <- c(xmin, xmax, ymin, ymax)

  #set crs
  crs(gt) <- "epsg:28992"

  #names
  names(gt) <- c("r", "g", "b")

  #return
  return(gt)
}
