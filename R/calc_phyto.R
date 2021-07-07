#' Rough indication of required years of phyto-remediation
#'
#' Calculate require years of phyto-remediation based on initial P-AL (empirical, roeghoorn) and PSDox in combination with Qmax (theoretical)
#'
#' @param dt data.table with soil data (topsoil only)
#' @param loc #column distinguishing locations
#' @param pal Column of P-AL (mg P2O5/100g)
#' @param psd Column of PSDox (percentage)
#' @param qmax  Column of Qmax (mmol/kg)
#' @param pox Column of P-ox (mmol/kg)
#' @param OS  Column of organic matter (percentage)
#' @param thickness Column specificying thickness of soil layer (cm)
#' @param tpal P-AL target (mg P2O5/100g)
#' @param tpsd PSDox target (percentage)
#' @param extraction #Assumed P extraction by the crop (kg P2O5/ha/year)
#'
#' @examples
#' calc_phyto <- loadbasemap(dt = topsoildata, loc = "locatie", pal = "P-AL (mg P2O5/100g)", psd = "FVG (%)", qmax = "Qmax (mmol/kg)",
#' pox = "P-ox (mmol/kg)",  OS = "Z_OM1_OF", thickness = "thick", tpal = 5, tpsd = 7, extraction = 70)
#'

#' @export


calc_phyto <- function(dt, loc, pal, psd, qmax, pox, OS, thickness, tpal, tpsd, extraction){
  #copy data.table
  dt <- data.table::copy(dt)

  #select rel columns
  relcols <- c(loc, pal, psd, qmax, pox, OS, thickness)
  dt <- dt[, ..relcols]

  #calc delta PAL based on initial pal----
  dt[, `Delta PAL uitmijnen (mg P2O5/100g/jaar)` := 0.048 * get(pal) - 0.213]

  #loop
  r1 <- list() #list to bind results to
  for(i in unique(dt[[loc]])){
    paljaren <- data.table::data.table()
    dt.pal <- dt[get(loc) == i]
    loop <- 0
    while(dt.pal[[pal]] > tpal){
      loop <- loop+1
      dt.pal[[pal]] <- dt.pal[[pal]] - dt.pal$`Delta PAL uitmijnen (mg P2O5/100g/jaar)`
    }
    paljaren[, loc := i]
    paljaren[, jaren := loop]
    r1[[i]] <- paljaren
  }
  pal.result <- do.call(rbind, r1)
  pal.result$method <- "intial PAL"

  #calc uitmijnjaren based on psdox----
  #reclaculate pox to mg P/kg
  dt[, `P-ox (mg P/kg)` := get(pox) * 30.973762]
  dt[[pox]] <- NULL

  #calc bulk density  based on organic matter
  dt[, `Dichtheid (g/cm3)` := 1 / (0.02525 * get(OS) + 0.6541)]
  dt[, `Dichtheid (kg/cm3)` := `Dichtheid (g/cm3)` / 1000]
  dt[, `Dichtheid (kg/m3)` := `Dichtheid (kg/cm3)` * 1000000]
  dt$`Dichtheid (g/cm3)` <- NULL
  dt$`Dichtheid (kg/cm3)` <- NULL

  #calc kg/soil/ha
  dt[, `thick (m)` := thick/100]
  dt[, `m3/soil/ha` := `thick (m)` * 10000]
  dt[, `kg soil/ha` := `m3/soil/ha` * `Dichtheid (kg/m3)`]

  #recalculate gewasonttrekking from kg P2O5/ha/jaar to kg P2O5/kg/jaar
  dt[, `Delta Pox (kg P2O5/kg/jaar)` := Extraction / `kg soil/ha`]

  #recalculate geasonttrekking to mg P/kg/jaar
  dt[, `Delta Pox (mg P/kg/jaar)` := `Delta Pox (kg P2O5/kg/jaar)` * (1/2.29) * 1000000]
  dt$`kg soil/ha` <- NULL
  dt$`Dichtheid (kg/m3)` <- NULL
  dt$`Delta Pox (kg P2O5/kg/jaar)` <- NULL
  dt$thick <- NULL
  dt$`m3/soil/ha` <- NULL

  #calc uitmijnjaren
  r2 <- list() #list to bind results to
  for(i in unique(dt[[loc]])){
    dt.ox <- dt[get(loc) == i]
    loop <- 0
    psdjaren <- data.table::data.table()
    while(dt.ox[[psd]] > tpsd){
      loop <- loop+1
      dt.ox[, `P-ox (mg P/kg)` := `P-ox (mg P/kg)` - `Delta Pox (mg P/kg/jaar)`]
      dt.ox[, `P-ox (mmol P/kg)` := `P-ox (mg P/kg)` / 30.973762]
      dt.ox[[psd]] <- dt.ox$`P-ox (mmol P/kg)` / dt.ox[[qmax]] * 100
    }
    psdjaren[, loc := i]
    psdjaren[, jaren := loop]
    r2[[i]] <- psdjaren
  }
  psd.result <- do.call(rbind, r2)
  psd.result$method <- "Pox icm Qmax"

  #bind results and return----
  result <- rbind(pal.result, psd.result)
  result <- data.table::as.data.table(tidyr::pivot_wider(result, names_from = method, values_from = jaren))
  return(result)
}
