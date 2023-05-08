#' Retrieve AHN height map
#'
#' Retrieve AHN3 height map (5m or 50cm resolution) for polygon of interest
#' @param sf Spatial feature polygon
#' @param expand Extension in meters
#' @param type String stating which AHN data to select ("5m_dtm", "50cm_dtm" and "50cm_dsm") currently supported)
#' @param savedir Directory to save rasters to
#' @param tmpdir Optional: String stating a folder within dev which already exists where temporary AHN files can be downloaded to. Only relevant if type = "50cm_dtm".
#' @importFrom httr parse_url build_url
#' @importFrom ows4R WFSClient
#'
#' @examples
#' ahn <- loadahn(sf = parcel, expand = 20, mask = TRUE, type = "50cm_dtm")
#'
#' @export
loadahn <- function(sf, expand, type, savedir, tmpdir){
  #set expand to 0, mask to FALSE and type to 50cm_dtm when missing
  if (missing(expand) == TRUE) {
    expand <- 0
    print("Expand missing. Set to 0.")
  }

  if (missing(type) == TRUE) {
    type <- "50cm_dtm"
    print("type missing. Set to 5m_dtm.")
  }

  #some checks
  savedir <- gsub("/$", "", savedir)
  if(!savedir %in% list.dirs(recursive = FALSE, full.names = FALSE)){
    stop("savedir does not exist")
  }

  if(!type %in% c("5m_dtm", "50cm_dtm", "50cm_dsm")){
    stop(paste0("Type ", type, " is not correct. Choose 5m_dtm, 50cm_dtm or 50cm_dsm"))
  }

  #buffer sf with expand
  sf.buffered <- st_buffer(sf, dist = expand)

  #retrieve 5m_dtm when that is the desired AHN3 map
  if (type == "5m_dtm") {
    #retrieve user info, set folder of ahn
    user <- Sys.getenv()[["USERNAME"]]
    ahn.folder <-
      paste0(
        "C:/Users/",
        user,
        "/SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI/Sven Verweij - NMI-DATA/relief/ahn3_5m_dtm/raw"
      )

    #read kaartbladen
    ahn3 <-
      st_read(paste(ahn.folder, "/ahn_kaartbladen.gpkg", sep = ""), quiet = TRUE)

    #transform sf to crs of ahn
    sf.buffered <- sf.buffered |> st_transform(st_crs(ahn3))

    #intersect with kaartbladen
    rel.kaartbladen <-
      st_intersects(sf.buffered, ahn3) |> unlist() |> unique()

    #Make a list of the tiles and corresponding files
    ahn3 <- ahn3[rel.kaartbladen,]$bladnr
    ahn3 <- paste(ahn3, collapse = "|")
    ahn.list <-
      list.files(
        path = ahn.folder,
        pattern = ahn3,
        ignore.case = TRUE,
        full.names = TRUE
      )
  } else if (type == "50cm_dtm" | type == "50cm_dsm") {

    #create dev folder if not existing
    if (!dir.exists("dev")) {
      dir.create("dev")
    }

    #create temporary folder if not provided as input and not existing in /dev)
    if (!missing(tmpdir)) {
      tmpdir <- paste0("dev/", tmpdir)
    } else{
      tmpdir <- paste0("dev/ahn3_", type)
      tmpdir_addition <- 1

      if (!dir.exists(paste0("dev/ahn3_", type))) {
        dir.create(paste0("dev/ahn3_", type))
      } else{
        tmpdir_new <- tmpdir
        while (dir.exists(tmpdir_new)) {
          tmpdir_new <- paste0(tmpdir, "_", tmpdir_addition)
          tmpdir_addition <- tmpdir_addition + 1
        }
        dir.create(tmpdir_new)
        tmpdir <- tmpdir_new
      }
    }

    #read bladindex from pdok
    #set WFS url gathered from rest SPI
    wfs.url <- "https://service.pdok.nl/rws/ahn3/wfs/v1_0?"

    #build a request for getcapacabilities
    url <- httr::parse_url(wfs.url)
    url$query <- list(service = "WFS",
                      request = "GetCapabilities"
    )
    request <- httr::build_url(url)

    #check which layers are present (version 2.0.0 seen in xml)
    bwk_client <- ows4R::WFSClient$new(wfs.url,
                                serviceVersion = "2.0.0")
    bwk_names <- bwk_client$getFeatureTypes(pretty = TRUE)

    #grepl bladindex
    bwk_names <- bwk_names[grepl("bladindex", bwk_names)]$name

    #retrieve layer 'peilgebied_vigerend'----
    url <- httr::parse_url(wfs.url)
    url$query <- list(service = "wfs",
                      version = "2.0.0",
                      request = "GetFeature",
                      typename = bwk_names,
                      srsName = "EPSG:28992"
    )
    request <- httr::build_url(url)

    bladnrs <- read_sf(request)

    #transform crs aoi to crs bladnrs
    sf.buffered <- st_transform(sf.buffered, st_crs(bladnrs))

    #find bladnrs intersecting
    rel.bladnrs <- st_intersects(sf, bladnrs) |> unlist()
    rel.bladnrs <- bladnrs[rel.bladnrs, ]$bladnr
    rel.bladnrs <- unique(rel.bladnrs)

    #to upper to match pdok
    rel.bladnrs <- toupper(rel.bladnrs)

    #loop through bladnrs, download files to tmpdir
    length(rel.bladnrs)
    if (length(rel.bladnrs) == 1) {
      print(paste0("Downloading ", length(rel.bladnrs), " AHN file from PDOK"))
    } else{
      print(paste0("Downloading ", length(rel.bladnrs), " AHN files from PDOK"))
    }
    for (i in 1:length(rel.bladnrs)) {
      #show progress
      print(paste0("Downloading ", i, "/", length(rel.bladnrs), " AHN tile"))

      #only run if file does not exist
      if (!file.exists(paste0(tmpdir, "/", rel.bladnrs[i], ".ZIP")) &
          !file.exists(paste0(tmpdir, "/M_", rel.bladnrs[i], ".TIF"))) {
        #create url
        if(type == "50cm_dtm"){
           ahn.url <-
          paste0("https://download.pdok.nl/rws/ahn3/v1_0/05m_dtm/M_",
                 rel.bladnrs[i],
                 ".ZIP")
        } else if(type == "50cm_dsm"){
          ahn.url <-
            paste0("https://download.pdok.nl/rws/ahn3/v1_0/05m_dsm/R_",
                   rel.bladnrs[i],
                   ".ZIP")
        }


        #save .zip files in tmpdir
        download.file(
          ahn.url,
          destfile = paste0(tmpdir, "/", rel.bladnrs[i], ".ZIP"),
          method = "curl"
        )
      }
    }

    #unzip files
    print("Extracting .ZIP files")
    zipfiles <-
      list.files(tmpdir, pattern = ".ZIP$", full.names = TRUE)
    if (length(zipfiles) > 0) {
      for(i in 1:length(zipfiles)){
         #unzip
        unzip(zipfiles[i], exdir = tmpdir)

        #remove ZIP files (no longer required)
        file.remove(zipfiles[i])
      }
    }

    #create ahn.list
    ahn.list <-list.files(tmpdir, full.names = TRUE, pattern = ".TIF$")
  }

  #read corresponding raster files
  rasters_individual <- lapply(ahn.list, function(i) {
    terra::rast(i)
  })

  #merge rasters to one single raster (#m tov NAP)
  if (length(rasters_individual) == 1) {
    rasters_merged <- rasters_individual[[1]]
  } else if (length(rasters_individual) > 1) {
    rasters_merged <- do.call(terra::merge, rasters_individual)
  }

  #set name of raster
  names(rasters_merged) <- type

  #crop raster to extent
  raster.cropped <-
    terra::crop(rasters_merged, terra::ext(sf.buffered))

  #mask
  raster.masked <- terra::mask(raster.cropped, terra::vect(sf.buffered))

  #save
  terra::writeRaster(raster.cropped, filename = paste0(savedir, "/", type, "_cropped.tiff"))
  terra::writeRaster(raster.masked, filename = paste0(savedir, "/", type, "_masked.tiff"))
}


