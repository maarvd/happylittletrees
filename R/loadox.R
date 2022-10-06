#' Prep Eurofins oxalate data
#'
#' Retrieve p-ox, fe-ox and al-ox from eurofins .csv
#' @param file .csv file of Eurofins
#'
#' @examples
#' pox <- loadox("data/oxalate.csv")
#'

#' @export
loadox <- function(file){
  dt <- data.table::fread(file)

  #tidy colnames
  colnames(dt) <- tolower(colnames(dt))
  colnames(dt) <- gsub(" ", "", colnames(dt))

  #tidy OmMonster
  dt$ommonster <- gsub(" ", "", dt$ommonster)
  setnames(dt, old = "ommonster", new = "GSL_ID")

  #select rel cols
  dt <- dt[, c("GSL_ID", "p-ox", "fe-ox", "al-ox")]

  #change colnames
  setnames(dt, old = c("p-ox", "fe-ox", "al-ox"), new = c("P-ox (mmol/kg)", "Fe-ox (mmol/kg)", "Al-ox (mmol/kg)"))

  #return
  return(dt)
}
