#' @importFrom xcms groupval
#' @importFrom xcms featureValues
#' @importFrom utils read.csv
NULL

#' Check if input is a text file or xlsx file
#'
#' @param path path to the file

file_type <- function(path) {
  f <- file(path)
  ext <- summary(f)$class
  close.connection(f)
  ext
}

#' Create initial qcrepotobject structure from input variable
#'
#' @param data_file Rdata file containing XCMS output, object should be
#'  called xset
#' @param projectdir Folder where Rdata file is located and where output should
#'  be directed
#' @param author Character sring woth reports author name, optional
#' @param metaData_file Files in xlsx format containing sample names and class
#'  labels
#' @param raw_path File path where mzML files are located
#' @param InternalProjectRef Character string containing project reference,
#'  optional
#' @param Dataset Character string to describe data set, optional
#' @param Organisation Character string containing organisation name, optional
#' @param QC_label Label used for QC samples
#' @param Blank_label Label usef for QC samples
#' @param classLabel Columns name in meta data file to use for class labels,
#'  default value is Class
#' @param excludeQC Which QC samples to exclude from RSD\% calculations,
#'  signal batch correction and PCA plots
#' @param assay Names for analytical assay, if set to NULL name of the input
#'  Rdata file will be used
#' @param plot_eic Will create pdf fail with selected peak EIC's if set to TRUE
#' @param pca_scores_labels Defines if sample labels should be added to
#'  filtered data PCA scores plot
#' @param mv_filter_method Defines what method for MV filter
#' @param mv_filter_frac Threshod of faction of detection of MV filter
#' @param xcms_output Name of the R object in Rdata file containing XCMS outputs
#' @param msms_label Label used for MS/MS data files
#' @param group_names character(), character vector containing uique sample
#'  group identifiers in the raw data files. If left empty function will
#'  attemp to fill in class labels for blank and QC samples.
#' 
#' @export

createQCreportObject <- function(data_file,
  projectdir,
  author="",
  metaData_file=NULL,
  raw_path=NULL,
  InternalProjectRef="",
  Dataset="",
  Organisation="",
  QC_label="QC",
  Blank_label="Blank",
  classLabel="Class",
  excludeQC=c(1L:5L),
  assay=NULL,
  plot_eic=TRUE,
  pca_scores_labels="all",
  mv_filter_method="across",
  mv_filter_frac=0.5,
  xcms_output="xset",
  msms_label="MSMS",
  group_names=NULL) {

  QCreportObject <- list()

  QCreportObject$xcms_output <- xcms_output

  QCreportObject$pca_scores_labels <- pca_scores_labels
  QCreportObject$data_file <- data_file
  QCreportObject$projectdir <- projectdir
  QCreportObject$group_names <- group_names

  QCreportObject$filtering <- list()
  QCreportObject$filtering$mv_filter_method <- mv_filter_method
  QCreportObject$filtering$mv_filter_frac <- mv_filter_frac

  # load data and create peakMatrix from Rdata or tab-separated file

  setwd(QCreportObject$projectdir)

  if (file_type(QCreportObject$data_file) == "gzfile" ||
      grepl(".rdata", tolower(QCreportObject$data_file)) ||
      tolower(tools::file_ext(QCreportObject$data_file)) == ".rdata") {
    load(QCreportObject$data_file) # includes xset
    if (!exists(QCreportObject$xcms_output)) {
      stop("Please check that XCMS object set in xcms_ouput can't be located.")
    }

    if (is(get(QCreportObject$xcms_output), "xcmsSet")) {
      QCreportObject$xset <- get(QCreportObject$xcms_output)
      QCreportObject$xcms_class <- "xcmsSet"
      QCreportObject$peakMatrix <- xcms::groupval(object=QCreportObject$xset,
      method="medret", value="into", intensity="into")
    } else if (is(get(QCreportObject$xcms_output), "XCMSnExp")) {
      QCreportObject$xset <- get(QCreportObject$xcms_output)
      QCreportObject$xcms_class <- "XCMSnExp"
      QCreportObject$peakMatrix <- xcms::featureValues(object=QCreportObject$xset,
      method="medret", value="into", intensity="into")
    }

  } else {
    if (!file.exists(QCreportObject$data_file)) {
      stop("Specified input data file doesn't exist or can't be accessed!")
    }
    QCreportObject$peakMatrix <- as.matrix(read.csv(QCreportObject$data_file,
    header=TRUE, row.names=1L, check.names=FALSE))
    QCreportObject$xcms_class <- "none"
  }

  projectInfo <- list()
  projectInfo$author <- author
  projectInfo$InternalProjectRef <- InternalProjectRef
  projectInfo$Dataset <- Dataset
  projectInfo$Organisation <- Organisation

  if (is.null(assay)) {
    projectInfo$assay <- sub(pattern=".Rdata", "", QCreportObject$data_file)
  } else {
    projectInfo$assay <- assay
  }

  QCreportObject$projectInfo <- projectInfo
  QCreportObject$projectdir <- projectdir

  metaData <- list()
  metaData$file <- metaData_file
  metaData$classColumn <- classLabel

  QCreportObject$metaData <- metaData

  QCreportObject$raw_path <- raw_path

  QCreportObject$QC_label <- QC_label
  QCreportObject$msms_label <- msms_label
  QCreportObject$Blank_label <- Blank_label

  QCreportObject$pdfout <- file.path(QCreportObject$projectdir,
    paste(QCreportObject$projectInfo$assay, "_EICs.pdf", sep=""))
  QCreportObject$xlsxout <- file.path(QCreportObject$projectdir,
    paste(QCreportObject$projectInfo$assay, ".xlsx", sep=""))

  QCreportObject$plots <- list()
  QCreportObject$tables <- list()
  QCreportObject$data <- list()

  QCreportObject$excludeQC <- excludeQC

  QCreportObject <- createProjectHeader(QCreportObject=QCreportObject)

  QCreportObject <- sampleSummary(QCreportObject=QCreportObject)

  QCreportObject <- sampleSummaryPlots(QCreportObject=QCreportObject)

  QCreportObject <- PCA(QCreportObject=QCreportObject)

  # RT shifts
  if (!is.null(QCreportObject$xset)) {
    QCreportObject <- RTstabilityAssement(QCreportObject=QCreportObject)
  }

  if (!is.null(QCreportObject$xset) & plot_eic == TRUE) {
    QCreportObject <- EICs(QCreportObject=QCreportObject)
  }

  QCreportObject <- missingValues(QCreportObject=QCreportObject)

  QCreportObject <- RSDstatistics(QCreportObject=QCreportObject)

  if (!is.null(QCreportObject$QC_label))
    QCreportObject <- QC(QCreportObject=QCreportObject)

  QCreportObject <- createXlsx(QCreportObject=QCreportObject)

  if (!is.null(QCreportObject$QC_label))
    QCreportObject <- SignalBatchCorrection(QCreportObject=QCreportObject)

  QCreportObject
}
