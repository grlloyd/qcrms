#' @import rmarkdown
#' @import captioner
#' @import pmp
#' @import sbc
#' @import gridExtra
#'
NULL

#' Create a pdf output from data slots on QCreportObject
#'
#' @param QCreportObject Qcreport object
#' @param outputfile Name of the output pdf file
#' @export


createQCreport <- function (QCreportObject, outputfile=NULL)
{
  if (is.null(outputfile))
  {
    outputfile <- paste(QCreportObject$projectInfo$assay,"pdf",sep=".")
  }

  source (paste(system.file(package = "qcrms"),"captions","Captions.R",sep="/"))
  QCrmd <- paste(system.file(package = "qcrms"),"rmd","QC_report.Rmd",sep="/")

  render (QCrmd, output_format = "pdf_document", output_file = outputfile,
          output_dir = QCreportObject$projectdir,
          knit_root_dir=QCreportObject$projectdir,
          intermediates_dir = QCreportObject$projectdir, clean = T)

  unlink (paste(QCreportObject$projectdir,"functions", sep="/"),recursive = T, force = T)
  unlink (paste(QCreportObject$projectdir,"Processing_steps", sep="/"),recursive = T, force = T)
}
