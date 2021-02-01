#' The TreeTalkersCheck main function.
#'
#' This function allows you to execute the TreeTalkersCheck routine.
#' @param whichDay The day to filter data and produce reports? Defaults to Sys.Date()-1
#' @return  This function saves graphs, DBs and reports into "C:/TreeTalkerDB".
#' @keywords TreeTalkers
#' @export
#' @examples
#' TreeTalkersCheck()
#' TreeTalkersCheck("2020-09-16")

TreeTalkersCheck = function(whichDay=Sys.Date()-1){
#------------------------------------------------------------------------------#
checkRequirements(whichDay)
#------------------------------------------------------------------------------#
DownloadNaturetalkers()
#------------------------------------------------------------------------------#
dbs <- ReadServerData()
#------------------------------------------------------------------------------#
cloudCheck <- checkCloud(dbs[[1]], whichDay)
#------------------------------------------------------------------------------#
TTCheck <- checkTT(dbs[[2]], whichDay)
#------------------------------------------------------------------------------#
suppressWarnings(Plot(whichDay))
#------------------------------------------------------------------------------#
for (i in 1:2) {
suppressWarnings(
rmarkdown::render(
system.file("rmd", "render.Rmd", package = "TreeTalkersCheck"),
output_format = c("html_document", "word_document")[i],
output_file = paste0(whichDay, c(".html", ".docx"))[i],
output_dir = file.path(file.path("C:", "TreeTalkerDB"), "dailyReports")))}
#------------------------------------------------------------------------------#
}
