#' Retrieve landschappelijke bodemkaart (LBK)
#'
#' Retrieve landschappelijke bodemkaart for polygon of interest
#' @param sf Spatial feature polygon
#' @param expand Extension in meters
#'
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs st_cast st_crop st_as_sfc st_as_text
#'
#' @examples
#' lbk <- loadlbk(sf = parcel, expand = 20)
#'@export
loadlbk <- function(sf, expand) {
  #buffer sf object (transform to amersfoort first)
  sf <- st_transform(sf, crs = st_crs(28992))
  sf_buffered <- st_buffer(sf, expand)

  #check for NMI-DATA env
  nmi_data <- Sys.getenv("NMI-DATA")
  if(nmi_data == ""){
    stop("set NMI-DATA environment variable")
  }

  #create path to BOFEK .gdb
  lbk_path <- paste0(nmi_data, "bodem/LBK2025/RAW/LBK_Nederland_2025_25.gpkg")
  if(!file.exists(lbk_path)){
    stop("LBK file not found")
  }

  #read using a wkt filter
  wktfilter <- st_bbox(sf_buffered) |> st_as_sfc() |> st_as_text()
  lbk <- st_read(lbk_path, wkt_filter = wktfilter,
                 layer = "FG_Eenheid",
                 quiet = TRUE)

  #set geometry
  lbk <- st_set_geometry(lbk, 'geom')

  #crop
  lbk <- st_crop(lbk, sf_buffered) |> suppressWarnings()

  #return
  return(lbk)
}
