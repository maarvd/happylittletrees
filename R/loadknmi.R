#' Retrieve daily KNMI data for a specific weather station
#'
#' @param from_date Earliest date of the date range
#' @param to_date Latest date of the date range
#' @param stationID ID of the weather station
#' @param params Parameters of interest
#' @examples
#' prec_table <- loadknmi("2001.01.10", "2002.01.10", 240, c("RH", "RHX"))


loadknmi <- function(from_date, to_date, stationID, params){
  #set base URL
  baseURL <- "https://www.daggegevens.knmi.nl/klimatologie/daggegevens"

  #set date to a numeric value
  from_date <- as.numeric(gsub("\\.", "", from_date))
  to_date <- as.numeric(gsub("\\.", "", to_date))

  #create URL based on input
  URL <- paste0(baseURL, "?start=", from_date, "&end=", to_date, "&stns=", stationID)

  #read URL
  knmi_table <- suppressMessages(readr::read_csv(URL, comment = "#", col_names = FALSE))

  #set col names
  if(ncol(knmi_table) == 41){
    colnames(knmi_table) <- c("STN","YYYYMMDD","DDVEC","FHVEC","FG","FHX","FHXH","FHN","FHNH","FXX","FXXH","TG","TN","TNH","TX","TXH","T10N",
 "T10NH","SQ","SP","Q","DR","RH","RHX","RHXH","EV24","PG","PX","PXH","PN","PNH","VVN","VVNH","VVX","VVXH","NG",
 "UG","UX","UXH","UN","UNH")
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
