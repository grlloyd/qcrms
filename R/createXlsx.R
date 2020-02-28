#' @import openxlsx
NULL

#' This function format ions identifiers
#' @author G. Le Corguille
#' @param variableMetadata vairable meta data object
#' @param numDigitsRT decimal digits for RT
#' @param numDigitsMZ number of deciaml digits for mz

formatIonIdentifiers <- function(variableMetadata, numDigitsRT=0L,
  numDigitsMZ=0L) {
    splitDeco <- strsplit(as.character(variableMetadata$name), "_")
    idsDeco <- sapply(splitDeco, function(x) { deco <- unlist(x)[2L];
    if (is.na(deco)) return("") else return(paste0("_", deco)) })
    namecustom <- make.unique(paste0("M", round(variableMetadata[, "mz"],
      numDigitsMZ), "T", round(variableMetadata[, "rt"], numDigitsRT),
      idsDeco))
    variableMetadata <- cbind(name=variableMetadata$name, namecustom=namecustom,
      variableMetadata[, !(colnames(variableMetadata) %in% "name")])
    return(variableMetadata)
}


#' This function convert if it is required the Retention Time in minutes
#' @author G. Le Corguille
#' @param variableMetadata vairable meta data object
#' @param convertRTMinute convert RT in seconds to minutes

RTSecondToMinute <- function(variableMetadata, convertRTMinute) {
  if (convertRTMinute) {
    #converting the retention times (seconds) into minutes
    print("converting the retention times into minutes in the variableMetadata")
    variableMetadata[, "rt"] <- variableMetadata[, "rt"] / 60L
    variableMetadata[, "rtmin"] <- variableMetadata[, "rtmin"] / 60L
    variableMetadata[, "rtmax"] <- variableMetadata[, "rtmax"] / 60L
  }
  return(variableMetadata)
}

#' This function prepares peaklist object from xcms output
#' @author G. Le Corguille
#' @param xset XCMS object to process
#' @param intval Which value to use for peak area, see XCMS manual for more
#' details
#' @param convertRTMinute If RT shoudl be converted from seconds to minutes
#' @param numDigitsRT decimal digits for RT
#' @param numDigitsMZ number of deciaml digits for mz

#@Modified from W4M code by G. Le Corguille
# value: intensity values to be used into, maxo or intb
getPeaklistW4M <- function(xset, intval="into", convertRTMinute=FALSE,
  numDigitsMZ=4L, numDigitsRT=0L) {
    variableMetadata_dataMatrix <- xcms::peakTable(xset, method="medret",
      value=intval)
    variableMetadata_dataMatrix <- cbind(name=groupnames(xset),
      variableMetadata_dataMatrix)

  variableMetadata <-
    variableMetadata_dataMatrix[, !(
      make.names(colnames(variableMetadata_dataMatrix)) %in%
      c(make.names(sampnames(xset))))]
  variableMetadata <- RTSecondToMinute(variableMetadata, convertRTMinute)
  variableMetadata <- formatIonIdentifiers(variableMetadata,
    numDigitsRT=numDigitsRT, numDigitsMZ=numDigitsMZ)

  out <- list()
  out$variableMetaData <- variableMetadata
  out
}

#' Create xlsx output file from XCMS output
#'
#' @param QCreportObject Qcreport object

createXlsx <- function(QCreportObject) {

  if (!is.null(QCreportObject$xset)) {
    W4M <- getPeaklistW4M(xset=QCreportObject$xset)
    rownames(QCreportObject$peakMatrix) <- W4M$variableMetaData$name

  } else {
    QCreportObject$peakMatrix[QCreportObject$peakMatrix == 0L] <- NA
  }

  # Write out meta data
  # Columns sample_name, class, injection order

  # Put label "Removed" for qc samples used as leading/outliers during
  # processing in meta data column used for class label
  QCreportObject$metaData$table[, QCreportObject$metaData$classColumn] <-
    QCreportObject$metaData$samp_lab

  # W4M for no good reason decided to check if file names contains "-" and
  # similar symbols. Why?
  # If it xcms can handle original file names, and after all files have been
  # processed.
  # Where is tracability to the original files if this has been done
  # to the outputs?

  if (!all(colnames(QCreportObject$data$dataMatrix) ==
    QCreportObject$metaData$table$Sample)) {

    if (all(colnames(QCreportObject$data$dataMatrix) ==
      make.names(QCreportObject$metaData$table$Sample))) {
      colnames(QCreportObject$data$dataMatrix) <-
        QCreportObject$metaData$table$Sample
    } else {
        stop("Sample names present in XCMS output doesn't match with
          meta data.")
      }
  }

  wb <- createWorkbook()
  addWorksheet(wb, "dataMatrix")
  writeData(wb, "dataMatrix", QCreportObject$peakMatrix, rowNames=TRUE)

  addWorksheet(wb, "metaData")
  writeData(wb, "metaData", QCreportObject$metaData$table, rowNames=FALSE)
  if (!is.null(QCreportObject$xset)) {
    addWorksheet(wb, "variableMetaData")
    writeData(wb, "variableMetaData", W4M$variableMetaData, rowNames=FALSE)
  } else {
    addWorksheet(wb, "variableMetaData")
    variableMetaData <- rowMeans(QCreportObject$peakMatrix, na.rm=TRUE,
      dims=1L)
    writeData(wb, "variableMetaData", data.frame(mz=names(variableMetaData),
      intensity=as.vector(variableMetaData)), rowNames=FALSE)
  }

  saveWorkbook(wb, QCreportObject$xlsxout, overwrite=TRUE)

  QCreportObject
}
