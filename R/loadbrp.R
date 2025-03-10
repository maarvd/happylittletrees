#' Retrieve BRP
#'
#' Retrieve brp gewaspercelen for aoi using the bbox of an sf object
#' @param sf Spatial feature object
#' @param year 2009 - 2024 supported. Defaults to 2019.
#' @param expand Extension in meters. Defaults to 100 meters.
#'
#' @importFrom sf st_read st_buffer st_intersects st_transform st_bbox st_crs st_as_sfc st_set_geometry st_as_text
#'
#' @examples
#' brp <- loadbrp(sf = parcels, year = 2019, expand = 100)

#' @export
loadbrp <- function(sf, expand, year) {
  if(missing(expand) == TRUE){
    expand <- 100
    print("Expand missing. Set to 100 meter (default).")
  }
  if(missing(year) == TRUE){
    year <- 2019
    print("Year missing. Set to 22019 (default).")
  }
  #create a wkt filter
  sf.bbox <- st_transform(sf, 28992) |> st_bbox() |> st_as_sfc() |> st_as_sf()  |> st_set_geometry("geom")
  sf.bbox <- st_buffer(sf.bbox, expand)
  wkt_filter <- st_as_text(sf.bbox$geom)

  #find the sven directory
  dirlist <- list.dirs(paste0("C:/Users/", Sys.info()[["user"]]), recursive = FALSE)
  dirlist <- dirlist[grepl("SPRINGG", dirlist)]
  dirlist <- dirlist[!grepl("OneDrive", dirlist)]
  rel_dir <- dirlist[lapply(
    dirlist,
    FUN =  function(x) {
      tmp <- list.dirs(x, recursive = FALSE)
      tmp <- grepl("Sven", tmp)
      tmp <- any(tmp == TRUE)
      return(tmp)
    }
  ) |> unlist()]
  rel_dir <- paste0(rel_dir,
                    "/Sven Verweij - NMI-DATA/landgebruik/brp/products/")

  #list the BPR products
  brplist <- list.files(rel_dir, full.names = TRUE, pattern = ".gpkg$")

  #omit duplicates and DESKTOP
  brplist <- brplist[!grepl("DESKTOP|LAPTOP|2020_concept|2021_concept|2022_concept",
                            brplist)]

  #grepl the relevant year
  rel_brp <- brplist[grepl(year, brplist)]

  #read with the created wkt_filter
  rel_brp <- st_read(rel_brp, wkt_filter = wkt_filter)
  st_crs(rel_brp) <- st_crs(28992)

  #intersects
  intersect_matrix <- st_intersects(sf, rel_brp, sparse = TRUE)
  intersect_rows <- intersect_matrix |> unlist() |> unique()
  rel_brp <- rel_brp[intersect_rows, ]

  #return
  return(rel_brp)
}
