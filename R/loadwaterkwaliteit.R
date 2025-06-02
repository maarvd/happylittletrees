#' Retrieve waterkwaliteitsdata
#'
#' Retrieve waterkwaliteitsdata from waterkwaliteitsportaal (local dump)
#' @param sf Spatial feature object
#' @param expand Extension in meters. Defaults to 20 meters.
#'
#' @importFrom sf st_read st_buffer st_transform st_bbox st_crs st_as_sf st_intersects
#' @import data.table
#' @importFrom janitor remove_empty
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr group_by
#'
#' @examples
#' water <- loadwaterkwaliteit(sf = parcels, expand = 10000)

#' @export
loadwaterkwaliteit <- function(sf, expand){
  #buffer
  sf_buffered <- st_buffer(sf, expand)
  sf_buffered_bbox <- st_bbox(sf_buffered)

  #areas
  bbox_gebieden <- structure(list(gebied = c("hoogheemraadschapdestichtserijnlanden.",
                            "hoogheemraadschaphollandsnoorderkwartier.", "hoogheemraadschapvandelfland.",
                            "hoogheemraadschapvanrijnland.", "hoogheemraadschapvanschielandenkrimpenerwaard.",
                            "rijkswaterstaat.", "waterschapaaenmaas.", "waterschapamstelgooienvecht.",
                            "waterschapbrabantsedelta.", "waterschapdedommel.", "waterschapdrentsoverijsselsedelta.",
                            "waterschaphollandsedelta.", "waterschaphunzeenaas.", "waterschaplimburg.",
                            "waterschapnoorderzijlvest.", "waterschaprijnenijssel.", "waterschaprivierenland.",
                            "waterschapscheldestromen.", "waterschapvalleienveluwe.", "waterschapvechtstromen.",
                            "waterschapzuiderzeeland.", "wetterskipfryslan."), geom = structure(list(
                              structure(list(structure(c(109991, 157747, 157747, 109991,
                                                         109991, 464152, 464152, 440176, 440176, 464152), dim = c(5L,
                                                                                                                  2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
                                                                                                                    structure(c(101895, 101895, 278026.09, 278026.09, 101895,
                                                                                                                                488064, 577192, 577192, 488064, 488064), dim = c(5L,
                                                                                                                                                                                 2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
                                                                                                                                                                                   structure(c(67987, 93705, 93705, 67987, 67987, 461941,
                                                                                                                                                                                               461941, 435117, 435117, 461941), dim = c(5L, 2L))), class = c("XY",
                                                                                                                                                                                                                                                             "POLYGON", "sfg")), structure(list(structure(c(81604, 118788,
                                                                                                                                                                                                                                                                                                            118788, 81604, 81604, 494786, 494786, 446451.86, 446451.86,
                                                                                                                                                                                                                                                                                                            494786), dim = c(5L, 2L))), class = c("XY", "POLYGON", "sfg"
                                                                                                                                                                                                                                                                                                            )), structure(list(structure(c(88025, 118470, 118470, 88025,
                                                                                                                                                                                                                                                                                                                                           88025, 456206, 456206, 434227, 434227, 456206), dim = c(5L,
                                                                                                                                                                                                                                                                                                                                                                                                   2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
                                                                                                                                                                                                                                                                                                                                                                                                     structure(c(13565.3999999985, 13565.3999999985, 278026.09,
                                                                                                                                                                                                                                                                                                                                                                                                                 278026.09, 13565.3999999985, 310000, 610300, 610300,
                                                                                                                                                                                                                                                                                                                                                                                                                 310000, 310000), dim = c(5L, 2L))), class = c("XY", "POLYGON",
                                                                                                                                                                                                                                                                                                                                                                                                                                                               "sfg")), structure(list(structure(c(133371, 200099, 200099,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   133371, 133371, 425846, 425846, 370897, 370897, 425846), dim = c(5L,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      structure(c(109949, 149390, 149390, 109949, 109949, 493633,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  493633, 456789, 456789, 493633), dim = c(5L, 2L))), class = c("XY",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "POLYGON", "sfg")), structure(list(structure(c(72499, 72499,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               278026.09, 278026.09, 72499, 375800, 414846, 414846, 375800,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               375800), dim = c(5L, 2L))), class = c("XY", "POLYGON", "sfg"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               )), structure(list(structure(c(128460, 174687, 174687, 128460,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              128460, 410337, 410337, 360162, 360162, 410337), dim = c(5L,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         structure(c(187176, 243731, 243731, 187176, 187176, 557450,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     557450, 469705, 469705, 557450), dim = c(5L, 2L))), class = c("XY",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "POLYGON", "sfg")), structure(list(structure(c(49845, 112958.31,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  112958.31, 49845, 49845, 439746, 439746, 408678, 408678,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  439746), dim = c(5L, 2L))), class = c("XY", "POLYGON", "sfg"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  )), structure(list(structure(c(225287, 277212, 277212, 225287,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 225287, 594071, 594071, 527395, 527395, 594071), dim = c(5L,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            structure(c(166707, 213480, 213480, 166707, 166707, 417467,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        417467, 307336, 307336, 417467), dim = c(5L, 2L))), class = c("XY",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "POLYGON", "sfg")), structure(list(structure(c(205750, 257838,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     257838, 205750, 205750, 608726, 608726, 552205, 552205, 608726
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ), dim = c(5L, 2L))), class = c("XY", "POLYGON", "sfg")),
                              structure(list(structure(c(187156, 261317, 261317, 187156,
                                                         187156, 477260, 477260, 4e+05, 4e+05, 477260), dim = c(5L,
                                                                                                                2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
                                                                                                                  structure(c(102962, 200209, 200209, 102962, 102962, 444883,
                                                                                                                              444883, 414020, 414020, 444883), dim = c(5L, 2L))), class = c("XY",
                                                                                                                                                                                            "POLYGON", "sfg")), structure(list(structure(c(14869, 77521,
                                                                                                                                                                                                                                           77521, 14869, 14869, 418020, 418020, 358936, 358936, 418020
                                                                                                                                                                                            ), dim = c(5L, 2L))), class = c("XY", "POLYGON", "sfg")),
                              structure(list(structure(c(145000, 209227.17, 209227.17,
                                                         145000, 145000, 503439, 503439, 440197, 440197, 503439), dim = c(5L,
                                                                                                                          2L))), class = c("XY", "POLYGON", "sfg")), structure(list(
                                                                                                                            structure(c(223181, 269170, 269170, 223181, 223181, 540505,
                                                                                                                                        540505, 463150, 463150, 540505), dim = c(5L, 2L))), class = c("XY",
                                                                                                                                                                                                      "POLYGON", "sfg")), structure(list(structure(c(138674, 195191,
                                                                                                                                                                                                                                                     195191, 138674, 138674, 539421, 539421, 474756, 474756, 539421
                                                                                                                                                                                                      ), dim = c(5L, 2L))), class = c("XY", "POLYGON", "sfg")),
                              structure(list(structure(c(125360, 223354, 223354, 125360,
                                                         125360, 611065, 611065, 535457, 535457, 611065), dim = c(5L,
                                                                                                                  2L))), class = c("XY", "POLYGON", "sfg"))), n_empty = 0L, crs = structure(list(
                                                                                                                    input = "EPSG:28992", wkt = "PROJCRS[\"Amersfoort / RD New\",\n    BASEGEOGCRS[\"Amersfoort\",\n        DATUM[\"Amersfoort\",\n            ELLIPSOID[\"Bessel 1841\",6377397.155,299.1528128,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4289]],\n    CONVERSION[\"RD New\",\n        METHOD[\"Oblique Stereographic\",\n            ID[\"EPSG\",9809]],\n        PARAMETER[\"Latitude of natural origin\",52.1561605555556,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",5.38763888888889,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",0.9999079,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",155000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",463000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting (X)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"northing (Y)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Engineering survey, topographic mapping.\"],\n        AREA[\"Netherlands - onshore, including Waddenzee, Dutch Wadden Islands and 12-mile offshore coastal zone.\"],\n        BBOX[50.75,3.2,53.7,7.22]],\n    ID[\"EPSG\",28992]]"), class = "crs"), class = c("sfc_POLYGON",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "sfc"), precision = 0, bbox = structure(c(xmin = 13565.3999999985,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ymin = 307336, xmax = 278026.09, ymax = 611065), class = "bbox"))), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           22L), class = c("sf", "data.frame"), sf_column = "geom", agr = structure(c(gebied = NA_integer_), class = "factor", levels = c("constant",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "aggregate", "identity")))

  #intersect
  rel_gebieden <- st_intersects(sf_buffered,
                bbox_gebieden) |> unlist() |> unique()
  rel_gebieden <- bbox_gebieden[rel_gebieden,]
  rel_gebieden <- rel_gebieden$gebied

  #portaal
  username <- Sys.info()[["user"]]
  portaaldir <- paste0("C:/Users/", username,  "/SPRINGG YAZILIM GELISTIRME TICARET LIMITED SIRKETI/Sven Verweij - NMI-DATA/waterkwaliteit/Waterkwaliteitsportaal_v2/output/meetgegevens/oppervlaktewater/")
  filelist <-  list.files(portaaldir,full.names = TRUE)

  #rel gebieden
  filelist <- filelist[grepl(paste0(rel_gebieden, collapse = "|"), filelist)]

  #select relevant data, omit many
  relcodes <- c(
    "PO4",
    #fosfaat
    "Ptot",
    #P-totaal
    "O2",
    #zuurstof
    "Cl",
    #chloride
    "NH4",
    #ammonium
    "NO2",
    #nitriet
    "NO3",
    #nitraat
    "Ntot",
    #N-totaal
    "NKj",
    #stikstof-kjehldahl
    "Fe",
    #ijzer
    "Corg",
    #organisch koolstof
    "SO4",
    #sulfaat
    "HCO3"
  ) #waterstofcarbonata

  #read and filter
  relwater <- lapply(filelist, FUN = function(x){
    #read
    water <- fread(x)
    water <- water[Parameter.code %in% c(relcodes)]

    #janitor to already remove empty rows and cols
    water <- janitor::remove_empty(water, which = c("rows", "cols"))

    #omit more non-required columns
    water <- water[, c("Meetobject.code", "Rapportagejaar",
              "Omschrijving",
              "GeometriePunt.X_RD",
              "GeometriePunt.Y_RD",
              "KRWwatertype.omschrijving",
              "WatergangCategorie.omschrijving",
              "Meetjaar",
              "Resultaatdatum",
              "Parameter.code",
              "Eenheid.code",
              "Numeriekewaarde")]


    #omit data where resultaatdatum is incorrectly reported
    water <- water[Resultaatdatum != "1900-01-01"]

    #omit columns which only contain "" (and thus not empty) and NA
    water <- water[, replace(.SD, .SD == "", NA)]
    water <- remove_empty(water, which = c("rows", "cols"))

    #tidy the parameter so eenheid is included
    water$parameter <- paste0(water$Parameter.code, " (", water$Eenheid.code, ")")
    water[, c("Parameter.code", "Eenheid.code") := NULL]

    #tidy additional columns
    colnames(water) <- tolower(colnames(water))
    colnames(water) <- gsub("geometriepunt.|_rd|.code", "", colnames(water))

    #omit duplicates
    water <- unique(water)

    #omit metingen which are outside of the bounding box of zuid-holland
    water <- water[x >= sf_buffered_bbox[1] & x < sf_buffered_bbox[3]]
    water <- water[y >= sf_buffered_bbox[2] & y < sf_buffered_bbox[4]]

    #convert to a wide format
    #nice for estimating ratios
    #saves file size before convertint o sf object
    water_wide <- water %>% group_by(meetobject,
                                         x,
                                         y,
                                         omschrijving,
                                         meetjaar,
                                         rapportagejaar,
                                         resultaatdatum,) %>%
      pivot_wider(
        names_from  = parameter,
        values_from = numeriekewaarde,
        values_fn = function(x) {
          mean(x, na.rm = TRUE)
        }
      )
    setDT(water_wide)

    #return
    return(water_wide)
  })
  relwater <- rbindlist(relwater,
                        fill = TRUE)

  #cast to an sf
  relwater.sf <- st_as_sf(relwater, coords = c("x", "y"),
                          crs = st_crs(28992))

  #return
  return(relwater.sf)
}

