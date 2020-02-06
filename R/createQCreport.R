#' @import rmarkdown
#' @import captioner
#' @import pmp
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
    outputfile <- paste(QCreportObject$projectInfo$assay, "pdf", sep=".")
  }

  source (file.path(system.file(package = "qcrms"), "captions", "Captions.R"))
  QCrmd <- file.path(system.file(package = "qcrms"), "rmd", "QC_report.Rmd")

  render (QCrmd, output_format = "pdf_document", output_file = outputfile,
          output_dir = QCreportObject$projectdir,
          knit_root_dir = QCreportObject$projectdir,
          intermediates_dir = QCreportObject$projectdir, clean = T)

  unlink (file.path(QCreportObject$projectdir, "functions"),recursive = T, force = T)
  unlink (file.path(QCreportObject$projectdir, "Processing_steps"),recursive = T, force = T)
}
