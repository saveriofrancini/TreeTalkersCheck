#' Read server data function
#'
#' Adjusts and merges the 4B-4C and 4D-49 DBs and produces two files, one for TTs (ttDB.csv) and one for clouds (cloudDB.csv), both stand by for further analysis.
#' @return  A list with the cloudDB and the ttDB.
#' @keywords TreeTalkers
#' @export
#' @examples
#' ReadServerData()

ReadServerData <- function(){

dbname <- "C:/TreeTalkerDB/TT_CloudDB.csv"
cat("Cleaning the", dbname, "file and organizing the data into the C:/TreeTalkerDB/ttDB.csv and the C:/TreeTalkerDB/CloudDB.csv files.",  "\n")

# reading servers data
serversData <- read.table(dbname, sep = ";")

# manage V2 column
cutV2 <- strsplit(as.character(serversData$V2), ",")
# select just the second element
ttId <- do.call(c, lapply(cutV2, function(x){x[2]}))

sitoId <- serversData[,ncol(serversData)]

serversData <- cbind(sitoId, ttId, serversData[,4:(ncol(serversData)-1)])

# We have 4 different databases
dbString <- list("4B", "4C", "4D", "49")
db4B <- serversData[serversData$V4=="4B",]
db4C <- serversData[serversData$V4=="4C",]

db4D <- serversData[serversData$V4=="4D",]
db49 <- serversData[serversData$V4=="49",]

## DB cloud
# Columns rename
colnames(db4B) <- c("Location", "cloudId", "stringType", "timeStamp",
                    "accumulatedRecords", "pendingRecords", "MCC",
                    "MNC", "GSMregistration", "GSMfield", "Battery", "firware")
db4B <- db4B[,1:12]

# remove the columns with empty timeslot otherwise R sees them as a record
db4C$V7 <- NULL

colnames(db4C) <- c("Location", "cloudId", "stringType", "timeStamp",
                    "TBLlocked", paste0("Sensor", 1:64))

# merge cloud different strings (4B and 4C)
cloudDB <- merge(db4B,db4C, by = c('cloudId','timeStamp'))
cloudDB <- cloudDB[!duplicated(cloudDB),]
cloudDB_filename <- file.path("C:", "TreeTalkerDB", "CloudDB.csv")

write.table(cloudDB, cloudDB_filename, sep = ";", row.names = F)

# DB TT
#rename
colnames(db4D) <- c("Location", "TTsn", "stringType", "timeStamp",
                    "tref_0", "theat_0", "growth", "adc_bandgap", "nbits",
                    "relativeHumidity", "temperature", "gzm", "gzsd", "gym",
                    "gysd", "gxm", "gxsd", "tref_1", "theat_1", "stwc", "adc_vbat")
db4D <- db4D[,1:21]
colnames(db49) <- c("Location", "TTsn", "stringType", "timeStamp",
                    paste0("B", 1:12), "intTime", "gain")
db49 <- db49[,1:18]
# merge TT different strings (4D and 49)
ttDB <- merge(db4D,db49, by = c('TTsn','timeStamp'))
ttDB <- ttDB[!duplicated(ttDB),]

# add the TTId to the db
TT_code <- read.table(system.file("parameters", "TTId.txt", package = "TreeTalkersCheck"),
                      sep = ";", header = T)
colnames(TT_code)[1] <- "ttid"
TTsn <- as.character(ttDB$TTsn)
TTid <- TT_code$ttid[match(TTsn, TT_code$TTSn)]
ttDB <- cbind(TTid, ttDB)

ttDB_filename <- file.path("C:", "TreeTalkerDB", "ttDB.csv")
write.table(ttDB, ttDB_filename, sep = ";", row.names = F)

return(list(cloudDB, ttDB))
}
