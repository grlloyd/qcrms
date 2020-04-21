#' @import openxlsx
NULL

#' This function prepares peaklist object from xcms output
#' @author G. Le Corguille
#' @param xset XCMS object to process
#' @param intval Which value to use for peak area, see XCMS manual for more
#' details

#@Modified from W4M code by G. Le Corguille
# value: intensity values to be used into, maxo or intb
getPeaklistW4M <- function(xset, intval="into") {
    variableMetadata_dataMatrix <- xcms::peakTable(xset, method="medret",
      value=intval)
    variableMetadata_dataMatrix <- cbind(name=groupnames(xset),
      variableMetadata_dataMatrix)

  variableMetadata <-
    variableMetadata_dataMatrix[, !(
      colnames(variableMetadata_dataMatrix) %in%
      c(make.names(sampnames(xset))))]

  variableMetadata
}

#' Create xlsx output file from XCMS output
#'
#' @param QCreportObject Qcreport object

createXlsx <- function(QCreportObject) {

  if (!is.null(QCreportObject$xset)) {
    variableMetaData <- getPeaklistW4M(xset=QCreportObject$xset)
    rownames(QCreportObject$peakMatrix) <- variableMetaData$name

  } else {
    QCreportObject$peakMatrix[QCreportObject$peakMatrix == 0L] <- NA
  }

  # Write out meta data
  # Columns sample_name, class, injection order

  # Put label "Removed" for qc samples used as leading/outliers during
  # processing in meta data column used for class label
  QCreportObject$metaData$table[, QCreportObject$metaData$classColumn] <-
    QCreportObject$metaData$samp_lab

  wb <- createWorkbook()
  addWorksheet(wb, "dataMatrix")
  writeData(wb, "dataMatrix", QCreportObject$peakMatrix, rowNames=TRUE)

  addWorksheet(wb, "metaData")
  writeData(wb, "metaData", QCreportObject$metaData$table, rowNames=FALSE)
  if (!is.null(QCreportObject$xset)) {
    addWorksheet(wb, "variableMetaData")
    writeData(wb, "variableMetaData", variableMetaData, rowNames=FALSE)
  } else {
    addWorksheet(wb, "variableMetaData")
    variableMetaData <- rowMeans(QCreportObject$peakMatrix, na.rm=TRUE,
      dims=1L)
    # BeamsPy specific input, has to have columns: name, mz, intensity and rt.
    writeData(wb, "variableMetaData", data.frame(name=names(variableMetaData),
        mz=names(variableMetaData), intensity=as.vector(variableMetaData),
        rt=0), rowNames=FALSE)
  }

  saveWorkbook(wb, QCreportObject$xlsxout, overwrite=TRUE)

  QCreportObject
}
