#' Check requirements function
#'
#' Prepare data and handle any exception that can occur each time a new user runs the TreeTalkersCheck package for the first time. Also saves a "whichDay.txt" temporary file.
#' @param whichDay The day to filter data and produce reports? Defaults to Sys.Date()-1
#' @return  This function saves a file: file.path(tempdir(), "whichDay.txt")
#' @keywords TreeTalkers
#' @export
#' @examples
#' checkRequirements()
#' checkRequirements("2020-09-16")

checkRequirements <- function(whichDay = Sys.Date()-1){

  # checking input files
  if(isFALSE(file.exists(
    system.file("parameters", "serverLinks.txt", package = "TreeTalkersCheck")
  )
  )
  ) {
    stop("The C:/TreeTalkersCheck/inputParameters.txt file is missing")
  }

  if(isFALSE(file.exists(
    system.file("parameters", "TTId.txt", package = "TreeTalkersCheck")
  )
  )
  ) {
    stop("The C:/TreeTalkersCheck/TTId.txt file is missing")
  }

  Rdir <- list.files(file.path("C:", "Program Files", "R"), full.names = T)
  Rdir <- Rdir[length(Rdir)]
  Rpath <- file.path(Rdir, "bin", "Rscript.exe")
  if(isFALSE(file.exists(Rpath))) {
    stop(paste0("The ", Rpath, " file is missing"))
  }

  # a whichDay.txt file is required to state the selected day in the reports
  writeLines(as.character(whichDay), file.path(tempdir(), "whichDay.txt"))

  # Loading required packages
  required_packages <- c(
    "lubridate", "ggplot2", "grid", "knitr", "rmarkdown"
  )
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if (length(missing_packages)!=0) install.packages(missing_packages, repos = "http://cran.us.r-project.org")
  suppressWarnings(suppressPackageStartupMessages(
    noPrint <- lapply(required_packages, library, character.only = T)
  ))

  # enviroment variables
  if(dir.exists("C:/Program Files/RStudio/bin/pandoc")){
    Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
  }

}
