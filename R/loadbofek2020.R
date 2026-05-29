#' Retrieve BOFEK cluster from bodemdata WFS service
#'
#' Retrieve BOFEK2020 cluster for polygon of interest
#' @param sf Spatial feature polygon
#' @param expand Extension in meters
#'
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs st_cast st_crop st_as_sfc st_as_text
#' @importFrom data.table setnames as.data.table
#' @importFrom readxl read_xlsx
#'
#' @examples
#' bofek <- loadbofek2020(sf = parcel, expand = 20)


#'@export
loadbofek2020 <- function(sf, expand) {
  #buffer sf object (transform to amersfoort first)
  sf <- st_transform(sf, crs = st_crs(28992))
  sf_buffered <- st_buffer(sf, expand)

  #check for NMI-DATA env
  nmi_data <- Sys.getenv("NMI-DATA")
  if(nmi_data == ""){
    stop("set NMI-DATA environment variable")
  }

  #create path to BOFEK .gdb
  bofek_gdb <- paste0(nmi_data, "bodem/alterra/BOFEK2020/raw/GIS/BOFEK2020_bestanden/BOFEK2020.gdb")
  if(!file.exists(bofek_gdb)){
    stop("BOFEK2020 geodatabase not found")
  }

  #read using a wkt filter
  wktfilter <- st_bbox(sf_buffered) |> st_as_sfc() |> st_as_text()
  bofek <- st_read(bofek_gdb, wkt_filter = wktfilter)

  #set geometry
  bofek <- st_set_geometry(bofek, 'geom')

  #crop
  bofek <- st_crop(bofek, sf_buffered) |> suppressWarnings()

  #to single polygons
  bofek <- st_cast(bofek, 'POLYGON') |> suppressWarnings()

  #crop
  bofek <- st_crop(bofek, st_bbox(sf_buffered)) |> suppressWarnings()

  #tidy columns
  bofek$Shape_Length <- NULL
  bofek$Shape_Area <- NULL
  data.table::setnames(
    bofek,
    old = c("BOFEK2020"),
    new = c("bofek2020_cluster")
  )

  #read tabel met omschrijvingen
  omschrijving_file <- paste0(nmi_data, "bodem/alterra/BOFEK2020/raw/GIS/BOFEK2020_bestanden/tabellen/Clusterhoofden.xlsx")
  if(!file.exists(omschrijving_file)){
    stop("Tabel met omschrijvingen niet gevonden op dataschijf")
  }
  omschrijving <- readxl::read_xlsx(omschrijving_file) |> data.table::as.data.table()

  #tidy
  omschrijving <- omschrijving[, c("clust1", "Omschrijving cluster")]
  omschrijving <- omschrijving[!is.na(clust1)]
  if(any(duplicated(omschrijving$clust1)) == TRUE){
    stop("Duplicates in omschrijvingen van BOFEK clusters")
  }
  data.table::setnames(omschrijving, old = c("clust1", "Omschrijving cluster"),
           new = c("bofek2020_cluster", "bofek2020_omschrijving"))

  #join
  bofek <- merge(bofek, omschrijving, by = 'bofek2020_cluster', all.x = TRUE)

  #return
  return(bofek)
}
