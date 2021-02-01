#' Plot function
#'
#' Calculates and saves several graphs into "C:/TreeTalkerDB".
#' @param whichDay The day to filter data and produce reports? Defaults to Sys.Date()-1
#' @return  A list with the DB filtered on the selected wichDay and the alert DB with detected anomalies registered.
#' @keywords TreeTalkers
#' @export
#' @examples
#' Plot("2020-09-16l")

Plot <- function(whichDay = Sys.Date()-1){

outPathReport <- file.path(file.path("C:", "TreeTalkerDB"), "dailyReports")
if(dir.exists(outPathReport)==F) dir.create(outPathReport)

cat("Calculating and saving some graphics in", outPathReport,  "\n")

TTdirectory <- file.path("C:/", "TreeTalkerDB")
outPath <- file.path(TTdirectory, "dailyReports", paste0(whichDay, "_plots"))
if(dir.exists(outPath)==F)dir.create(outPath)

# reading data
TTdb_today <- read.table(file.path(TTdirectory, "dailyDB", paste0(whichDay, "_TT.csv")), header = T, row.names = NULL, sep = ";")
TTdb_today$timeStamp <- strftime(as.POSIXct(as.character(TTdb_today$timeStamp)), format="%H")

# library
library(ggplot2)
library(grid)

#Temperature trend in the different sites

ggplot(TTdb_today, aes(timeStamp, temperature, colour = Location.x), fill = TTid) +
 geom_point(size = 0.8) +
  xlab("Hour of the day")+
 theme(
    panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.4, linetype = 'solid',
                                    colour = "white"),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "lightgrey",
                                    colour = "darkblue",
                                    size = 1.2, linetype = "solid"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18,  vjust = 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.x = element_text(face="bold", color = "black",
                                       size=14, vjust = 1),
            axis.text.y = element_text(face="bold", color = "black",
                                       size=14)
            )

ggsave(file.path(outPath, "temperature.PNG"), width = 10, height = 6)

#Growth rate trend in the different sites

ggplot(TTdb_today, aes(timeStamp, growthRate, colour = Location.x), fill = TTid) +
  geom_point(size = 0.8) +
  xlab("Hour of the day") +
  theme(
    panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.4, linetype = 'solid',
                                    colour = "white"),
    panel.background = element_rect(fill = "lightgrey",
                                    colour = "darkblue",
                                    size = 1.2, linetype = "solid"),
    panel.grid.major.x = element_blank(),
    #plot.title = element_text(size=26, face="bold", vjust = 0, hjust = 0),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18,  vjust = 1),
    axis.text.x = element_text(face="bold", color = "black",
                                       size=14, vjust = 1),
            axis.text.y = element_text(face="bold", color = "black",
                                       size=14),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
    )

ggsave(file.path(outPath, "growthRate.PNG"), width = 10, height = 6)

#Sapflux trend in the different sites

ggplot(TTdb_today, aes(timeStamp, sapFluxDensity, colour = Location.x), fill = TTid) +
  geom_point(size = 0.8) +
  xlab("Hour of the day") +
  theme(
    panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.4, linetype = 'solid',
                                    colour = "white"),
    panel.background = element_rect(fill = "lightgrey",
                                    colour = "darkblue",
                                    size = 1.2, linetype = "solid"),
    panel.grid.major.x = element_blank(),
    #plot.title = element_text(size=26, face="bold", vjust = 0, hjust = 0),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18,  vjust = 1),
    axis.text.x = element_text(face="bold", color = "black",
                                       size=14, vjust = 1),
            axis.text.y = element_text(face="bold", color = "black",
                                       size=14),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
    )

ggsave(file.path(outPath, "sapFluxDensity.PNG"), width = 10, height = 6)


#Battery trend in the different sites

ggplot(TTdb_today, aes(timeStamp, batteryLevel, colour = Location.x), fill = TTid) +
  geom_point(size = 0.8) +
  xlab("Hour of the day") +
  theme(
    panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.4, linetype = 'solid',
                                    colour = "white"),
    panel.background = element_rect(fill = "lightgrey",
                                    colour = "darkblue",
                                    size = 1.2, linetype = "solid"),
    panel.grid.major.x = element_blank(),
    #plot.title = element_text(size=26, face="bold", vjust = 0, hjust = 0),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18,  vjust = 1),
    axis.text.x = element_text(face="bold", color = "black",
                                       size=14, vjust = 1),
            axis.text.y = element_text(face="bold", color = "black",
                                       size=14),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
    )

ggsave(file.path(outPath, "batteryLevel.PNG"), width = 10, height = 6)

# graphs per each site
siti <- as.character(unique(TTdb_today$Location.x))

for (sito_i in siti) {

  db_sito_i <- TTdb_today[TTdb_today$Location.x==sito_i,]

  # temperature

  ggplot(db_sito_i, aes(x=timeStamp, y=temperature, group=TTid)) +
    geom_line(aes(color=TTid))+
    geom_point(aes(color=TTid))+
    #scale_y_continuous(limits = c(0, 30)) +
    xlab("Hour of the day") +
    theme(
      panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                      colour = "white"),
      panel.grid.minor = element_line(size = 0.4, linetype = 'solid',
                                      colour = "white"),
      panel.background = element_rect(fill = "lightgrey",
                                      colour = "darkblue",
                                      size = 1.2, linetype = "solid"),
      panel.grid.major.x = element_blank(),
      #plot.title = element_text(size=26, face="bold", vjust = 0, hjust = 0),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18,  vjust = 1),
      axis.text.x = element_text(face="bold", color = "black",
                                         size=14, vjust = 1),
              axis.text.y = element_text(face="bold", color = "black",
                                         size=14),
      legend.title = element_text(size = 13, face="bold"),
      legend.text = element_text(size = 15)
      )

  ggsave(file.path(outPath, paste0("temperature", sito_i, ".PNG")), width = 10, height = 6)

  # sapFlux

  ggplot(db_sito_i, aes(x=timeStamp, y=sapFluxDensity, group=TTid)) +
    geom_line(aes(color=TTid))+
    geom_point(aes(color=TTid))+
    #scale_y_continuous(limits = c(-2, 8)) +
    xlab("Hour of the day") +
    theme(
      panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                      colour = "white"),
      panel.grid.minor = element_line(size = 0.4, linetype = 'solid',
                                      colour = "white"),
      panel.background = element_rect(fill = "lightgrey",
                                      colour = "darkblue",
                                      size = 1.2, linetype = "solid"),
      panel.grid.major.x = element_blank(),
      #plot.title = element_text(size=26, face="bold", vjust = 0, hjust = 0),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18,  vjust = 1),
      axis.text.x = element_text(face="bold", color = "black",
                                         size=14, vjust = 1),
              axis.text.y = element_text(face="bold", color = "black",
                                         size=14),
      legend.title = element_text(size = 13, face="bold"),
      legend.text = element_text(size = 15)
      )

  ggsave(file.path(outPath, paste0("sapFluxDensity", sito_i, ".PNG")), width = 10, height = 6)

  # TT battery level

  ggplot(db_sito_i, aes(x=timeStamp, y=batteryLevel, group=TTid)) +
    geom_line(aes(color=TTid))+
    geom_point(aes(color=TTid))+
    #scale_y_continuous(limits = c(3.2, 4.4)) +
    xlab("Hour of the day") +
    theme(
      panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                      colour = "white"),
      panel.grid.minor = element_line(size = 0.4, linetype = 'solid',
                                      colour = "white"),
      panel.background = element_rect(fill = "lightgrey",
                                      colour = "darkblue",
                                      size = 0.8, linetype = "solid"),
      panel.grid.major.x = element_blank(),
      #plot.title = element_text(size=26, face="bold", vjust = 0, hjust = 0),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18,  vjust = 1),
      axis.text.x = element_text(face="bold", color = "black",
                                         size=14, vjust = 1),
              axis.text.y = element_text(face="bold", color = "black",
                                         size=14),
      legend.title = element_blank(),
      legend.text = element_text(size = 15)
      )

  ggsave(file.path(outPath, paste0("batteryLevel", sito_i, ".PNG")), width = 10, height = 6)

}

#growth rate graph per each TT
outPiante <- file.path(outPath, "growthRatePerTT")
if (dir.exists(outPiante)==F) dir.create(outPiante)

piante <- as.character(unique(TTdb_today$TTid))

for (pianta_i in piante) {

  db_pianta_i <- TTdb_today[TTdb_today$TTid==pianta_i,]
  db_pianta_i$timeStamp <- as.double(db_pianta_i$timeStamp)

  growthRateMin <- min(db_pianta_i$growthRate)
  growthRateMax <- max(db_pianta_i$growthRate)

  # scaleFUN <- function(x) sprintf("%.2f", x)
  scaleFUN <- function(x) {
  (x-growthRateMin)/(growthRateMax-growthRateMin)
  }

  db_pianta_i$growthRate <- scaleFUN(db_pianta_i$growthRate)

  if(growthRateMin == growthRateMax){
    db_pianta_i$growthRate <- rep(0.5, length(db_pianta_i$growthRate))
  }

  #plot

  ggplot(db_pianta_i, aes(x=timeStamp, y=growthRate, group=TTid)) +
    geom_line()+
    geom_point(size = 2.2)+
    #scale_y_continuous(limits = c(-1, 1)) +
    theme(
      panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                      colour = "white"),
      panel.grid.minor = element_line(size = 0.4, linetype = 'solid',
                                      colour = "white"),
      panel.background = element_rect(fill = "lightgrey",
                                      colour = "darkblue",
                                      size = 2, linetype = "solid"),
      panel.grid.major.x = element_blank(),
      #plot.title = element_text(size=26, face="bold", vjust = 0, hjust = 0),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(face="bold", color = "black",
                                         size=32, vjust = 1),
              axis.text.y = element_text(face="bold", color = "black",
                                         size=32)
      ) +
    annotation_custom(grobTree(textGrob(paste0("TT ", pianta_i), x=0.5,  y=0.95,
                                        gp=gpar(col="darkblue", fontsize=34, fontface="bold"))))+
    annotation_custom(grobTree(textGrob(paste0("Min = ", round(growthRateMin, 3)), x=0.25,  y=0.2,
                                        gp=gpar(col="darkblue", fontsize=30))))+
    annotation_custom(grobTree(textGrob(paste0("Max = ", round(growthRateMax, 3)), x=0.25,  y=0.1,
                                        gp=gpar(col="darkblue", fontsize=30))))+
    scale_y_continuous(breaks = seq(0, 1, 0.2) )+
    scale_x_continuous (limits = c(0, 24), breaks = seq(0, 24, 6) )

  ggsave(file.path(outPiante, paste0("growthRate", pianta_i, ".PNG")), width = 10, height = 6)
}

# plot cloud
# reading data
cloud_db_today <- read.table(file.path(TTdirectory, "dailyDB", paste0(whichDay, "_cloud.csv")), header = T, row.names = NULL, sep = ";")
cloud_db_today$timeStamp <- strftime(as.POSIXct(as.character(cloud_db_today$timeStamp)), format="%H")

# Battery trend in the different sites

ggplot(cloud_db_today, aes(timeStamp, Battery, colour = Location.x), fill = Location.x) +
  geom_point(size = 2) +
  xlab("Hour of the day")+
  theme(
    panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.4, linetype = 'solid',
                                    colour = "white"),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "lightgrey",
                                    colour = "darkblue",
                                    size = 1.2, linetype = "solid"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18,  vjust = 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.x = element_text(face="bold", color = "black",
                               size=14, vjust = 1),
    axis.text.y = element_text(face="bold", color = "black",
                               size=14)
  )

ggsave(file.path(outPath, "cloudBattery.PNG"), width = 10, height = 6)

}
