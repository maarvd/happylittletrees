#' Retrieve AHN height map
#'
#' Retrieve AHN4 DEM for area of interest
#' @param sf Spatial feature object
#' @param product AHN product ("dtm_05m", "dsm_05m", "dtm_5m", "dsm_5m")
#' @param expand Extension in meters
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs
#' @importFrom terra rast
#'
#' @examples
#' ahn <- loadahn(sf = parcel, expand = 20)
#'
#' @export
loadahn <- function(sf, product, expand) {
  #set expand to 0, mask to FALSE and type to 50cm_dtm when missing
  if (missing(expand) == TRUE) {
    expand <- 0
    print("Expand missing. Set to 0.")
  }
  if (missing(product) == TRUE) {
    product <- "dtm_05m"
    print("Product missing. Set to dtm_05m.")
  }
  if (!product %in% c("dtm_05m", "dsm_05m", "dtm_5m", "dsm_5m")) {
    stop("Product not recognized")
  }

  #create buffered sf object, standardize crs
  sf.transformed <- st_transform(sf, crs = st_crs(28992))
  sf.buffered <- st_buffer(sf.transformed, dist = expand)

  #download bladwijzer if not exists (for 5m products)
  if(product %in% c("dtm_5m", "dsm_5m")) {
    if (!dir.exists("dev")) {
      dir.create("dev")
    }
    if (!dir.exists("dev/ahn")) {
      dir.create("dev/ahn")
    }
    if (!file.exists("dev/ahn/bladwijzer.gpkg")) {
      print("downloading bladwijzer, save to dev")
      bladwijzer_url <- "https://basisdata.nl/hwh-ahn/AUX/bladwijzer.gpkg"
      download.file(
        bladwijzer_url,
        destfile = "dev/ahn/bladwijzer.gpkg",
        method = 'curl',
        quiet = TRUE
      )
    }

    #read bladwijzer
    bladwijzer <- st_read("dev/ahn/bladwijzer.gpkg", quiet = TRUE)

    #create intersects with bladwijzer
    rel.bladwijzers <- st_intersects(sf.buffered, bladwijzer) |> unique() |> unlist()
    rel.bladwijzer <- bladwijzer[rel.bladwijzers,]

    #select products
    if(product == "dtm_5m"){
      rel.files <- rel.bladwijzer$AHN4_5M_M
    } else if(product == "dsm_5m"){
      rel.files <- rel.bladwijzer$AHN4_5M_R
    }

    #download the files
    print(paste0("Downloading ", length(rel.files), " AHN tiles"))
    for (i in 1:length(rel.files)) {
      print(paste0(i, ":", length(rel.files)))
      download.file(
        url = rel.files[i],
        destfile = paste0("dev/ahn/", i, ".zip"),
        method = 'auto',
        quiet = TRUE
      )
    }

    #list all .zip files and extract
    print("Extracting .zip files")
    filelist <- list.files("dev/ahn", pattern = ".zip$", full.names = TRUE)
    lapply(
      filelist,
      FUN = function(x) {
        unzip(zipfile = x, exdir = "dev/ahn")
      }
    )

    #read all ahn files
    ahnfiles <- list.files("dev/ahn", pattern = "TIF$", full.names = TRUE)
    ahnfiles <- lapply(ahnfiles, rast)

    #merge
    print("Merging ahn tiles")
    ahn4 <- do.call(terra::merge, ahnfiles)

    #crop to aoi
    ahn4 <- crop(ahn4, sf.buffered)

    #assign a name
    if(product == "dtm_5m"){
      names(ahn4) <- "ahn4_5m_dtm"
    } else if(product == "dsm_5m"){
      names(ahn4) <- "ahn4_5m_dsm"
    }
  } else{
    #Check capabilities for name, version, format, etc....
    ahn4_capabilities <- "https://service.pdok.nl/rws/ahn/wcs/v1_0?SERVICE=WCS&request=GetCapabilities"
    #browseURL(ahn4_capabilities)

    #set path and variable of interest
    wcs_path <- "https://service.pdok.nl/rws/ahn/wcs/v1_0?SERVICE=WCS"
    wcs_request <- "REQUEST=GetCoverage"
    version <- "VERSION=2.0.1"
    coverageid = paste0("COVERAGEID=", product)
    maxsize = "MAXSIZE=10000"
    format <- "FORMAT=image/tiff"

    #set bbox
    xmin <- st_bbox(sf.buffered)[1] |> round(0)
    xmax <- st_bbox(sf.buffered)[3] |> round(0)
    ymin <- st_bbox(sf.buffered)[2] |> round(0)
    ymax <- st_bbox(sf.buffered)[4] |> round(0)
    boundingbox <- paste0("subset=X(",
                          xmin,
                          ",",
                          xmax,
                          ")&subset=y(",
                          ymin,
                          ",",
                          ymax,
                          ")")

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
    if (product == "dtm_05m") {
      names(ahn4) <- "ahn4_50cm_dtm"
    } else if (product == "dsm_05m") {
      names(ahn4) <- "ahn4_50cm_dsm"
    }
  }

  #return
  return(ahn4)
}
