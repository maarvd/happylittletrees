#' Retrieve BOFEK cluster from bodemdata WFS service
#'
#' Retrieve BOFEK2020 cluster for polygon of interest
#' @param sf Spatial feature polygon
#' @param expand Extension in meters
#'
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs st_cast
#' @importFrom data.table setnames
#'
#' @examples
#' bofek <- loadbofek2020(sf = parcel, expand = 20)


#'@export
loadbofek2020 <- function(sf, expand) {
  #buffer sf object (transform to amersfoort first)
  sf <- st_transform(sf, crs = st_crs(28992))
  sf_buffered <- st_buffer(sf, expand)

  #wfs capabilities
  wfs_capabilities <-
    "https://bodemdata.nl/geoserver/bodem/wfs?SERVICE=WFS&VERSION=1.3.0&REQUEST=GetCapabilities"
  #browseURL(wfs_capabilities)

  #set path and variable of interest
  wfs_path <- "https://bodemdata.nl/geoserver/bodem/wfs?"
  wfs_name <- "name=bodem:BOFEK_2020"
  wfs_service <- "SERVICE=WFS"
  wfs_version <- "VERSION=1.3"
  wfs_request <- "REQUEST=GetFeature"
  wfs_format <- "outputFormat=GML2"
  wfs_typename <- "typeName=bodem:BOFEK_2020"

  #set bbox
  xmin <- st_bbox(sf_buffered)[1] |> round(0)
  xmax <- st_bbox(sf_buffered)[3] |> round(0)
  ymin <- st_bbox(sf_buffered)[2] |> round(0)
  ymax <- st_bbox(sf_buffered)[4] |> round(0)
  wfs_bbox <- paste0("bbox=", xmin, ",", ymin, ",",  xmax, ",", ymax)

  #create path
  wfs <- paste(
    wfs_path,
    wfs_name,
    wfs_service,
    wfs_version,
    wfs_request,
    wfs_format,
    wfs_bbox,
    wfs_typename,
    sep = "&"
  )

  #read as sf object
  soil <- st_read(wfs, quiet = TRUE)

  #to single polygons
  soil <- st_cast(soil, 'POLYGON') |> suppressWarnings()

  #crop
  soil <- st_crop(soil, st_bbox(sf_buffered)) |> suppressWarnings()

  #tidy
  soil <- st_set_geometry(soil, 'geom')
  soil$gml_id <- NULL
  soil$OBJECTID_1 <- NULL
  soil$SHAPE_AREA <- NULL
  soil$SHAPE_LENG <- NULL
  data.table::setnames(
    soil,
    old = c("BOFEK2020", "OMSCHR"),
    new = c("bofek2020_cluster", "bofek2020_omschrijving")
  )

  #return
  return(soil)

}
