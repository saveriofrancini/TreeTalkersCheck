#' Check Cloud function
#'
#' Finds anomalies and prepare data for reports.
#' @param dbCloud The CloudDB returned by ReadServerData()
#' @param whichDay The day to filter data and produce reports? Defaults to Sys.Date()-1
#' @return  A list with the DB filtered on the selected wichDay and the alert DB with detected anomalies registered.
#' @keywords TreeTalkers
#' @export
#' @examples
#' checkTT(dbCloud, Sys.Date()-1)


checkCloud <- function(dbCloud, whichDay = Sys.Date() - 1) {
  cat("Checking the status of clouds on", paste0(whichDay),  "\n")

  # dbCloudToday
  dbCloud$timeStamp <-
    lubridate::as_datetime(dbCloud$timeStamp)# + 7200
  dbCloudToday <- as.Date(dbCloud$timeStamp) == whichDay
  dbCloudToday <- dbCloud[dbCloudToday,]

  # remove the columns with empty timeslots otherwise R recognize them as a record
  colonneFullZero <- apply(dbCloudToday, 2, function(x) {
    any(x != 0)
  })
  colonneFullZero[is.na(colonneFullZero)] <- FALSE
  dbCloudToday <- dbCloudToday[, colonneFullZero]

  # input parameters
  minimumGSM <- 9                   # condition_1
  maximumNumberOfPengindItems <- 10 # condition_2
  minimumBattery <- 4000            # condition_3
  minimumTalkerSignal <- -100       # condition_4

  # check for factors
  for (i in 16:ncol(dbCloudToday)) {
    dbCloudToday[, i] <- as.double(as.character(dbCloudToday[, i]))
  }

  # verifying the conditions
  condition_1 <- dbCloudToday$GSMfield > minimumGSM
  condition_2 <-
    dbCloudToday$pendingRecords < maximumNumberOfPengindItems
  condition_3 <- dbCloudToday$Battery > minimumBattery
  lowSignalSensorsList <- list()
  for (i in 1:nrow(dbCloudToday)) {
    Sensors_row_i <-
      as.double(as.character(dbCloudToday[i, 16:ncol(dbCloudToday)]))
    lowSignalSensors <- Sensors_row_i < minimumTalkerSignal
    lowSignalSensors[is.na(lowSignalSensors)] <- FALSE
    whichSensorsLowSignal <-
      (1:(ncol(dbCloudToday) - 14))[lowSignalSensors]
    lowSignalSensorsList[[i]] <- whichSensorsLowSignal
  }

  # extract the checking info

  # GSM field
  segnale <- dbCloudToday[condition_1 == FALSE,]
  segnale <- segnale[, c(1, 2, 3, 9)]

  # pending records
  if (length(condition_2) > 0) {
    pendingRecords <- dbCloudToday[condition_2 == FALSE,]
    pendingRecords <- pendingRecords[, c(1, 2, 3, 6)]
  } else{
    pendingRecords <- dbCloudToday[FALSE, c(1, 2, 3, 6)]
  }
  # battery level
  battery <- dbCloudToday[condition_3 == FALSE,]
  battery <- cbind(battery[, c(1, 2, 3)], battery$Battery)

  # RSSI signal
  lowSignalSensorsList <-
    lapply(lowSignalSensorsList, function(x) {
      paste(x, collapse = ";")
    })
  lowSignalSensors <- unlist(lowSignalSensorsList)

  lowSignalSensorsDb <- dbCloudToday[lowSignalSensors != "",]
  lowSignalSensorsDb <- lowSignalSensorsDb[, c(1, 2, 3)]
  lowSignalSensorsDb <-
    cbind(lowSignalSensorsDb, lowSignalSensors[lowSignalSensors != ""])
  colnames(lowSignalSensorsDb)[4] <- "whichSensor"

  colnames(segnale)[4] <- "issue"
  colnames(pendingRecords)[4] <- "issue"
  colnames(battery)[4] <- "issue"
  colnames(lowSignalSensorsDb)[4] <- "issue"
  tipology <- c(
    rep("cloudSignal", nrow(segnale)),
    rep("pendingRecords", nrow(pendingRecords)),
    rep("battery", nrow(battery)),
    rep("sensorSignal", nrow(lowSignalSensorsDb))
  )

  out <- rbind(segnale, pendingRecords, battery, lowSignalSensorsDb)
  out <- cbind(tipology, out$issue, out[, 1:3])
  colnames(out)[2] <- "issue"

  out_dir <- file.path("C:", "TreeTalkerDB", "dailyDB")
  if (dir.exists(out_dir) == F)
    dir.create(out_dir)

  write.table(out,
              file.path(out_dir, paste0(whichDay, "_cloudAlert.csv")),
              row.names = FALSE,
              sep = ";")
  write.table(dbCloudToday,
              file.path(out_dir, paste0(whichDay, "_cloud.csv")),
              row.names = FALSE,
              sep = ";")

  return(list(dbCloudToday, out))
}
