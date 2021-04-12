#' Check TreeTalkers function
#'
#' Finds anomalies and prepare data for reports.
#' @param dbTT The ttDB returned by ReadServerData()
#' @param whichDay The day to filter data and produce reports? Defaults to Sys.Date()-1
#' @return  A list with the DB filtered on the selected wichDay and the alert DB with detected anomalies registered.
#' @keywords TreeTalkers
#' @export
#' @examples
#' checkTT(TTdb, Sys.Date()-1)

checkTT <- function(dbTT, whichDay = Sys.Date() - 1) {
  whichDay <- as.Date(whichDay)

  cat("Checking the status of TTs on", paste0(whichDay),  "\n")

  # dbTT_whichDay
  dbTT$timeStamp <- lubridate::as_datetime(dbTT$timeStamp)# +7200
  #isToday <- as.Date(dbTT$timeStamp)== "2020-08-04"
  isToday <- as.Date(dbTT$timeStamp) ==  whichDay
  dbTT_Today <- dbTT[isToday, ]
  #dbTT_Today <- dbTT_Today[order(dbTT_Today$ltimeStamp),]
  dbTT_Today$temperature <- dbTT_Today$temperature / 10

  # battery calculation
  batteryLevel <-
    2 * 1100 * dbTT_Today$adc_vbat / dbTT_Today$adc_bandgap / 1000
  batteryLevel <- round(batteryLevel, 2)
  dbTT_Today <- cbind(dbTT_Today, batteryLevel)

  # battery level alert
  lowBatteryDB <- dbTT_Today[dbTT_Today$batteryLevel <= 3.5, ]
  lowBatteryDB <- lowBatteryDB[, c(1, 3, 39)]
  if (nrow(lowBatteryDB) > 0) {
    lowBatteryDB <-
      aggregate(lowBatteryDB[, 2:3], list(as.character(lowBatteryDB$TTid)), min)
    colnames(lowBatteryDB)[1] <- "ttId"
  }

  # sap flux density calculation
  FromDigitalNumberToTemperature = function(x) {
    127.6 - (0.006045 * x) + (0.000000126 * x ^ 2) - (0.00000000000115 * x ^
                                                        3)
  }
  dbTT_Today$tref_0 <-
    FromDigitalNumberToTemperature(dbTT_Today$tref_0)
  dbTT_Today$tref_1 <-
    FromDigitalNumberToTemperature(dbTT_Today$tref_1)
  dbTT_Today$theat_0 <-
    FromDigitalNumberToTemperature(dbTT_Today$theat_0)
  dbTT_Today$theat_1 <-
    FromDigitalNumberToTemperature(dbTT_Today$theat_1)

  # Metodo 1
  # DeltaTon <- dbTT_Today$theat_1 - dbTT_Today$tref_1
  # DeltaToff <- dbTT_Today$theat_0 - dbTT_Today$tref_0
  # dbTT_Today <- cbind(dbTT_Today, DeltaTon, DeltaToff)
  # DeltaT_max <-
  #   aggregate(DeltaTon, list(as.character(dbTT_Today$TTid)), max)
  #
  # #DeltaTMax has been calculated daily for each TT and then appied to each TT associating TTid and Location
  # colnames(DeltaT_max) <- c("Location", "DeltaTMax")
  # ....
  # ....
  # sapFluxDensity <-
  #   (118.99 * (((
  #     as.double(as.character(dbTT_Today$DeltaTMax)) /
  #       (as.double(as.character(
  #         dbTT_Today$DeltaTon
  #       )) -
  #         as.double(as.character(
  #           dbTT_Today$DeltaToff
  #         )))
  #   ) - 1) ^ 1.231))


  # Metodo 2
  DeltaT_heat <- dbTT_Today$theat_1 - dbTT_Today$theat_0
  DeltaT_max <-
    aggregate(DeltaT_heat, list(as.character(dbTT_Today$TTid)), max)
  DeltaTMax <-
    as.character(DeltaT_max$x)[match(as.character(dbTT_Today$TTid),
                                     as.character(DeltaT_max$Group.1))]
  # dbTT_Today <- cbind(dbTT_Today, DeltaT_heat, DeltaT_max)
  dbTT_Today <- cbind(dbTT_Today, DeltaT_heat)

  # sapFluxDensity <- 4.79 * 27.777 * ((as.double(DeltaTMax)-DeltaT_heat)/DeltaT_heat)
  sapFluxDensity <-
    4.79 * ((as.double(DeltaTMax) - DeltaT_heat) / DeltaT_heat)

  dbTT_Today <- cbind(dbTT_Today, sapFluxDensity)

  # growth rate calculation
  #growthRate <- 18.2823+(-0.0006*dbTT_Today$growth)+(6.9143e-009*dbTT_Today$growth^2)+(-3.0237e-14*dbTT_Today$growth^3)

  # stemDiameterVariation <-
  #   (0.000000008 * (dbTT_Today$growth ^ 2)) - (0.0016 * dbTT_Today$growth) +
  #   89.032

  stemDiameterVariation <-
    (237908.4541 + (-1.1171 * dbTT_Today$growth)) / (199.4330 + dbTT_Today$growth)

  dbTT_Today <- cbind(dbTT_Today, stemDiameterVariation)

  out_dir <- file.path("C:", "TreeTalkerDB", "dailyDB")
  if (dir.exists(out_dir) == F)
    dir.create(out_dir)

  write.table(lowBatteryDB,
              file.path(out_dir, paste0(whichDay, "_TTAlert.csv")),
              row.names = FALSE,
              sep = ";")
  write.table(dbTT_Today,
              file.path(out_dir, paste0(whichDay, "_TT.csv")),
              row.names = FALSE,
              sep = ";")

  return(list(dbTT_Today, lowBatteryDB))
}
