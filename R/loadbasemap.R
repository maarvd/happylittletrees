#' Retrieve RGB basemaps from PDOK
#'
#' Retrieve areal basemaps using the bbox of an sf object
#' @param sf Spatial feature object
#' @param year 2016 - 2023 supported. Defaults to most actual year.
#' @param res Spatial resolution (high or low). Defaults to high (8cm) for 2021-2023, low (25cm) for 2016-2021.
#' @param expand Extension in meters. Defaults to 20 meters.
#' @param zoom Zoom level. Defaults to 13
#'
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs
#' @importFrom terra rast crs
#' @importFrom httr GET write_disk
#'
#' @examples
#' basemap <- loadbasemap(sf = parcels, year = 2023, res = 'high', expand = 20, zoom = 16)

#' @export
loadbasemap <- function(sf, year, res, expand, zoom){
  #set expand to 20 when missing
  if(missing(expand) == TRUE){
    expand <- 20
    print("Expand missing. Set to 20 meter (default).")
  }

  #set year to most actual when missing
  if(missing(year) == TRUE){
    year <- "Actueel"
    print("Source missing. Set to Actueel_orthoHR 8cm resolution (default).")
  }

  #if year <2016, stop
  if(year <2016){
    stop("Only  years 2016-2023 supported")
  }

  #when res is missing, set to high when year is 2021-2023 or actueel, else to low
  if(missing(res) == TRUE){
    if(year %in% c(2021:2023, "Actueel")){
      res <- "high"
      print("res missing. Set to high (available for specified year)")
    } else{
      res <- "low"
      print("res missing. Set to low (high not available for specified year)")
    }
  }

  #also set res to low when years < 2021
  if(year < 2021){
    res <- "low"
    print("Set res to low (high not available for specified year)")
  }

  #tidy layer (name)
  if(res == "low"){
    wms_name <- paste0(year, "_ortho25")
  } else if(res == "high"){
    wms_name <- paste0(year, "_orthoHR")
  }

  #transform & buffer sf
  sf <- st_transform(sf, crs = st_crs(28992))
  sf_buffered <- st_buffer(sf, expand)

  #set zoom when missing
  if(missing(zoom) == TRUE){
   zoom <- 13
  }

  #capabilities
  wms_capabilities <- "https://service.pdok.nl/hwh/luchtfotorgb/wms/v1_0?&request=GetCapabilities&service=wms"
  #browseURL(wms_capabilities)

  #base url of wms service
  wms_baseurl <- "https://service.pdok.nl/hwh/luchtfotorgb/wms/v1_0?"

  #settings
  wms_request <- "request=GetMap"
  wms_service <- "service=wms"
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
                   wms_service,
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
  temporary_name_raw <- "basemap_temporary"
  temporary_name_valid <- "basemap_temporary"
  iteration <- 1
  while(file.exists(paste0("dev/", temporary_name_valid, ".tiff"))){
    temporary_name_valid <- paste0(temporary_name_raw, iteration)
    iteration <- iteration + 1
  }
  temporary_name_valid <- paste0(temporary_name_valid, ".tiff")
  httr::GET(wms_url, write_disk(paste0("dev/", temporary_name_valid)))  |> suppressMessages() |> invisible()

  #read
  basemap <- rast(paste0("dev/", temporary_name_valid), noflip = TRUE) |> suppressWarnings() #extent will be set

  #read extent
  ext(basemap) <- c(xmin, xmax, ymin, ymax)

  #set crs
  crs(basemap) <- "epsg:28992"

  #names
  names(basemap) <- c("r", "g", "b")

  #return
  return(basemap)
}
