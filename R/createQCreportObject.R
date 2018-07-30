#' Create initial qcrepotobject structure from input variable
#'
#' @param FILENAME_RDATA Rdata file containing XCMS output, object should be called xset
#' @param projectdir Folder where Rdata file is located and where output should be directed
#' @param author Character sring woth reports author name, optional
#' @param metaData_file Files in xlsx format containing sample names and class labels
#' @param raw_path File path where mzML files are located
#' @param InternalProjectRef Character string containing project reference, optional
#' @param Dataset Character string to describe data set, optional
#' @param Organisation Character string containing organisation name, optional
#' @param QC_label Label used for QC samples
#' @param Blank_label Label usef for QC samples
#' @param classLabel Columns name in meta data file to use for class labels, default value is Class
#' @param excludeQC Which QC samples to exclude from RSD\% calculations, signal batch correction and PCA plots
#' @param assay Names for analytical assay, if set to NULL name of the input Rdata file will be used
#' @export


createQCreportObject <- function(FILENAME_RDATA,
                                 projectdir,
                                 author="",
                                 metaData_file,
                                 raw_path = NULL,
                                 InternalProjectRef="",
                                 Dataset="",
                                 Organisation="",
                                 QC_label="QC",
                                 Blank_label="Blank",
                                 classLabel="Class",
                                 excludeQC=c(1:5),
                                 assay=NULL
)
{

  QCreportObject <- list()

  QCreportObject$FILENAME_RDATA <- FILENAME_RDATA

  projectInfo <- list ()
  projectInfo$author <- author
  projectInfo$InternalProjectRef <- InternalProjectRef
  projectInfo$Dataset <- Dataset
  projectInfo$Organisation <- Organisation
  if (is.null(assay))
  {
    projectInfo$assay <- sub(pattern = ".Rdata","",QCreportObject$FILENAME_RDATA)
  } else
  {
    projectInfo$assay <- assay
  }

  QCreportObject$projectInfo <- projectInfo
  QCreportObject$projectdir <- projectdir

  #QCreportObject$codebase <- codebase

  metaData <- list ()
  metaData$file <- metaData_file
  metaData$classColumn <- classLabel

  QCreportObject$metaData <- metaData

  QCreportObject$raw_path <- raw_path

  QCreportObject$QC_label <- QC_label
  QCreportObject$Blank_label <- Blank_label

  QCreportObject$pdfout <- paste0(QCreportObject$projectdir,"/", QCreportObject$projectInfo$assay,"_EICs.pdf")
  QCreportObject$xlsxout <- paste0(QCreportObject$projectdir,"/", QCreportObject$projectInfo$assay,".xlsx")

  QCreportObject$plots <- list()
  QCreportObject$tables <- list()
  QCreportObject$data <- list()

  QCreportObject$excludeQC <- excludeQC

  QCreportObject <- qcrms::createProjectHeader(QCreportObject = QCreportObject)
  QCreportObject <- qcrms::sampleSummary(QCreportObject = QCreportObject)
  QCreportObject <- qcrms::sampleSummaryPlots(QCreportObject = QCreportObject)
  QCreportObject <- qcrms::PCA(QCreportObject = QCreportObject)

  QCreportObject <- qcrms::EICs(QCreportObject = QCreportObject)

  QCreportObject <- qcrms::missingValues(QCreportObject = QCreportObject)
  QCreportObject <- qcrms::RSDstatistics(QCreportObject = QCreportObject)#
  if (!is.null(QCreportObject$QC_label)) QCreportObject <- qcrms::QC(QCreportObject = QCreportObject)
  QCreportObject <- qcrms::createXlsx(QCreportObject = QCreportObject)
  if (!is.null(QCreportObject$QC_label)) QCreportObject <- qcrms::SignalBatchCorrection(QCreportObject = QCreportObject)

  QCreportObject
}

