#' Download function
#'
#' Appends all DBs at the specified server links in a single file ("TT_CloudDB.csv") which is updated with new records each time the TreeTalkersCheck package is executed.
#' @return
#' @keywords TreeTalkers
#' @export
#' @examples
#' DownloadNaturetalkers()

DownloadNaturetalkers <- function() {
  # read input parameters
  inpuParametersPath <-
    system.file("parameters", "serverLinks.txt", package = "TreeTalkersCheck")
  suppressWarnings(inpuParameters <-
                     read.table(inpuParametersPath, header = T))
  inpuParameters$server <- as.character(inpuParameters$server)
  inpuParameters$site   <- as.character(inpuParameters$site)
  inpuParameters$cloud  <- basename(dirname(inpuParameters$server))

  # Downlaod
  outdir <- file.path("C:", "TreeTalkerDB")
  outfile <- file.path("C:", "TreeTalkerDB", "TT_CloudDB.csv")
  if (file.exists(outfile))
    file.remove(outfile)
  if (isFALSE(dir.exists(outdir))) {
    dir.create(outdir)
  }
  outFileNames <-
    paste0(outdir,
           "/",
           paste0(inpuParameters$cloud, "_", inpuParameters$site),
           ".txt")
  for (i in 1:nrow(inpuParameters))  {
    download.file(inpuParameters$server[i], outFileNames[i])
    file_i <-
     read.table(
       outFileNames[i],
       sep = ";",
       col.names = paste0("V", 1:70),
       fill = T
     )

    file_i <- cbind(file_i, inpuParameters$site[i])
    write.table(
     file_i,
     outfile,
     sep = ";",
     col.names = F,
     append = T
    )
    cat("The",
        inpuParameters$server[i],
        "file has been appended to",
        outfile,
        "\n")
  }

  # remove temporary files
  noprint <- lapply(outFileNames, file.remove)

}
