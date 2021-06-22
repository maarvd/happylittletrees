#' Retrieve daily KNMI data for a specific weather station
#'
#' @param from_date Earliest date of the date range (yyyy.mm.dd)
#' @param to_date Latest date of the date range (yyyy.mm.dd)
#' @param stationID ID of the weather station
#' @param params Parameters of interest
#' @examples
#' prec_table <- loadknmi("2001.01.10", "2002.01.10", 240, c("RH", "RHX"))


loadknmi <- function(from_date, to_date, stationID, params){
  #set base URL
  baseURL <- "https://www.daggegevens.knmi.nl/klimatologie/daggegevens"

  #set date to a numeric value
  from_date <- as.numeric(gsub("\\.", "", from_date))
  from_date <- as.numeric(gsub("_", "", from_date))
  from_date <- as.numeric(gsub("-", "", from_date))

  to_date <- as.numeric(gsub("\\.", "", to_date))
  to_date <- as.numeric(gsub("_", "", to_date))
  to_date <- as.numeric(gsub("-", "", to_date))

  #create URL based on input
  URL <- paste0(baseURL, "?start=", from_date, "&end=", to_date, "&stns=", stationID)

  #read URL
  knmi_table <- suppressMessages(readr::read_csv(URL, comment = "#", col_names = FALSE))

  #retrieve columns
    #read csv
  colss <- suppressWarnings(suppressMessages(readr::read_csv(URL, col_names = FALSE)))
    #change col name
  colnames(colss) <- "kolom"
    #create list to bind relevant lines to
  collist <- list()
  for(i in 1:60){
    #loop through rows
    tmp <- colss[i,]
    #grepl patterns
    if(grepl(pattern = "^#", tmp$kolom) == TRUE &
       grepl(pattern = "DDVEC|FHVEC|FG|FHX|FHXH|FHN|FHNH|FXX|FXXH|TG|TN|TNH|TX|TXH|T10N|T10NH|SQ|SP|Q|DR|RH|RHX|RHXH|PG|PX|PXH|PN|PNH|VVN|VVNH|VVX|VVXH|NG|UG|UX|UXH|UN|UNH|EV24", tmp$kolom) == TRUE &
       data.table::like(pattern = "LON(east)", tmp$kolom, fixed = TRUE) == FALSE){
      collist[[i]] <- tmp
    }
  }
    #bind list
  collist <- do.call(rbind, collist)
    #str_split, only keep relevant name
  collist <- sapply(strsplit(collist$kolom,"  "), `[`, 1)
    #tidy
  collist <- gsub("# ", "", collist)
    #remove STN (is always first)
  collist <- collist[collist != "STN"]
    #add STN and YYYYMMDD (always first)
  collist <- c("STN", "YYYYMMDD", collist)

  #set col names
  if(ncol(knmi_table) == length(collist)){
    colnames(knmi_table) <- collist
  } else{
    stop("Kolom aantal is niet gelijk aan 41")
  }

  #select relevant columns
  relcols <- c("YYYYMMDD", params)
  knmi_table <- knmi_table[relcols]

  #set to data.table
  data.table::setDT(knmi_table)

  #convert YYYMMDD to date
  data.table::setnames(knmi_table, old = "YYYYMMDD", new = "Datum")
  knmi_table$Datum <- lubridate::ymd(knmi_table$Datum)

  #return
  return(knmi_table)
}
