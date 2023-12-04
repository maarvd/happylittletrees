#' Retrieve AHN height map
#'
#' Retrieve AHN4 50cm dtm height map for area of interest
#' @param sf Spatial feature object
#' @param expand Extension in meters
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs
#' @importFrom terra rast
#'
#' @examples
#' ahn <- loadahn(sf = parcel, expand = 20)
#'
#' @export
loadahn <- function(sf, expand){
  #set expand to 0, mask to FALSE and type to 50cm_dtm when missing
  if (missing(expand) == TRUE) {
    expand <- 0
    print("Expand missing. Set to 0.")
  }

  #transform to 28992
  sf.transformed <- st_transform(sf, crs = st_crs(28992))

  #buffer sf with expand
  sf.buffered <- st_buffer(sf.transformed, dist = expand)

  #Check capabilities for name, version, format, etc....
  ahn4_capabilities <- "https://service.pdok.nl/rws/ahn/wcs/v1_0?SERVICE=WCS&request=GetCapabilities"
  #browseURL(ahn4_capabilities)

  #set path and variable of interest
  wcs_path <- "https://service.pdok.nl/rws/ahn/wcs/v1_0?SERVICE=WCS"
  wcs_request <- "REQUEST=GetCoverage"
  version <- "VERSION=2.0.1"
  coverageid = "COVERAGEID=dtm_05m"
  maxsize = "MAXSIZE=10000"
  format <- "FORMAT=image/tiff"

  #set bbox
  xmin <- st_bbox(sf.buffered)[1] |> round(0)
  xmax <- st_bbox(sf.buffered)[3] |> round(0)
  ymin <- st_bbox(sf.buffered)[2]|> round(0)
  ymax <- st_bbox(sf.buffered)[4]|> round(0)
  boundingbox <- paste0("subset=X(", xmin, ",", xmax, ")&subset=y(", ymin, ",", ymax, ")")

  #create path
  wcs <- paste(wcs_path,
               wcs_request,
               version,
               coverageid,
               boundingbox,
               format,
               sep = "&")

  #load raster
  ahn4 <- rast(wcs)

  #set names
  names(ahn4) <- "ahn4_50cm_dtm"

  #return
  return(ahn4)
}


