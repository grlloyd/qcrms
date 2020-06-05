#' @importFrom openxlsx createWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#' @importFrom openxlsx saveWorkbook
#' @importFrom xcms groups
#' @importFrom xcms groupnames
NULL

#' This function prepares peaklist object from xcms output
#' @author G. Le Corguille
#' @param xset XCMS object to process
#' @param intval Which value to use for peak area, see XCMS manual for more
#' details

#@Modified from W4M code by G. Le Corguille
# value: intensity values to be used into, maxo or intb
getPeaklistW4M <- function(xset, intval="into") {
    if (is(xset, "xcmsSet")){
        variableMetadata <- as.data.frame(xcms::groups(xset))
    } else if (is(xset, "XCMSnExp")){
        variableMetadata <- xcms::featureDefinitions(xset)
        variableMetadata <-
            as.data.frame(variableMetadata[,
                !colnames(variableMetadata) %in% c("peakidx")])
        rownames(variableMetadata) <- NULL
    }
    colnames(variableMetadata)[c(1, 4)] <- c("mz", "rt")
    variableMetadata <- cbind(name=xcms::groupnames(xset), variableMetadata)
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

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "dataMatrix")
  openxlsx::writeData(wb, "dataMatrix", QCreportObject$peakMatrix,
      rowNames=TRUE)

  addWorksheet(wb, "metaData")
  openxlsx::writeData(wb, "metaData", QCreportObject$metaData$table,
      rowNames=FALSE)
  if (!is.null(QCreportObject$xset)) {
    openxlsx::addWorksheet(wb, "variableMetaData")
    writeData(wb, "variableMetaData", variableMetaData, rowNames=FALSE)
  } else {
    openxlsx::addWorksheet(wb, "variableMetaData")
    variableMetaData <- rowMeans(QCreportObject$peakMatrix, na.rm=TRUE,
      dims=1L)
    # BeamsPy specific input, has to have columns: name, mz, intensity and rt.
    openxlsx::writeData(wb, "variableMetaData",
        data.frame(name=names(variableMetaData), mz=names(variableMetaData),
        intensity=as.vector(variableMetaData), rt=0), rowNames=FALSE)
  }

  openxlsx::saveWorkbook(wb, QCreportObject$xlsxout, overwrite=TRUE)
  QCreportObject
}
