#' Retrieve Bodemschat
#'
#' Retrieve Bodemschat for area of interest, rasterize to 25m spatial resolution
#' @param sf Spatial feature polygon
#' @param expand Extension in meters
#'
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs st_as_sfc st_as_text
#' @importFrom terra rast ext crs res rasterize
#'
#' @examples
#' bodemschat <- loadbodemschat(sf = aoi, expand = 0)
#'
#'@export
loadbodemschat <- function(sf, expand) {
  #set expand to 0 when missing
  if(missing(expand) == TRUE){
    expand <- 0
    print("Expand missing. Set to 0 meter (default).")
  }

  #filepath of bodemschat
  bodemschat <- paste0(Sys.getenv("NMI-DATA"), "bodem/bodemschat/products/BS6/BS6_2021.gpkg")

  #check for NMI-DATA env
  nmi_data <- Sys.getenv("NMI-DATA")
  if(nmi_data == ""){
    stop("set NMI-DATA environment variable")
  }
  #check if bodemschat file is present
  if(!file.exists(bodemschat)){
    stop("bodemschat file not found")
  }

  #buffer sf object (transform to amersfoort first)
  sf <- st_transform(sf, crs = st_crs(28992))
  sf_buffered <- st_buffer(sf, expand)


  #read using a wkt filter
  wktfilter <- st_bbox(sf_buffered) |> st_as_sfc() |> st_as_text()
  bodemschat <- st_read(bodemschat, wkt_filter = wktfilter, quiet = TRUE)

  #create a 25m resolution raster
  samplerast <- rast()
  crs(samplerast) <- "epsg:28992"
  ext(samplerast) <- ext(sf_buffered)
  res(samplerast) <- 25

  #rasterize the bodemschat parameters
  bodemschat$ref_id <- NULL
  relcols <- colnames(bodemschat)[!colnames(bodemschat) %in% c("geom", "geometry")]
  bodemschat_raster <- lapply(relcols, FUN = function(x){
    rasterize(bodemschat, samplerast, field = x, fun = 'mean', na.rm = TRUE)
  })

  #combine
  bodemschat_raster <- rast(bodemschat_raster)

  #return
  return(bodemschat_raster)
}
