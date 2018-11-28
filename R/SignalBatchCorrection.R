#' @import sbcms
#' @import pmp
#' @import openxlsx
#'
NULL

#' Apply signal batch correction
#'
#' @param QCreportObject Qcreport object
#' @export


SignalBatchCorrection <- function(QCreportObject)
{
  wb <- loadWorkbook(xlsxFile = QCreportObject$xlsxout)

  # Samples are reordered so QC indexes have beens changed as well!
  class <- QCreportObject$metaData$table[,which(colnames(QCreportObject$metaData$table)==QCreportObject$metaData$classColumn)]

  if (is.null(QCreportObject$metaData$table$batch)){
    
    QCreportObject$metaData$table$batch <- rep(1, length(class))
  }


  # Blank filter
  blank_filtered <- filter_peaks_by_blank(QCreportObject$data$dataMatrix, 20, class,
                                        blank_label=QCreportObject$Blank_label,
                                        qc_label = NULL, remove = FALSE)[[1]]

  # QC MV fraction filter
  # MV in QC samples
  MV_filtered <- filter_peaks_by_fraction(blank_filtered, min_frac = 0.8,
                                        classes=class, method = "QC", qc_label = QCreportObject$QC_label)[[1]]

  PCAinF2 <- prepareData(Data=MV_filtered, classes=class,
                       blank = QCreportObject$Blank_label, PQN=T, mv_impute = T,
                       glogScaling = T,
                       qc_label = QCreportObject$QC_label, ignorelabel = "Removed")


  samp_lab5 <- class
  samp_lab5[-c(which(class==QCreportObject$QC_label))] <- "Removed"

  PCAinQC2 <- prepareData(Data=MV_filtered, classes=samp_lab5,
                        blank = QCreportObject$Blank_label, PQN=F, mv_impute = T,
                        glogScaling = F,
                        qc_label = NULL, ignorelabel = "Removed")


  QCreportObject$plots$SBPCAbefore <- doPCA (Data=PCAinF2$Data, classes=PCAinF2$classes, PQN=T, mv_impute = T,
                                       glogScaling = T, scale=F,
                labels="QC", qc_label = QCreportObject$QC_label, plotTitle = "PCA, blank and QC MV filtered")


  QCreportObject$plots$SBPCAbeforeQC <- doPCA (Data=PCAinQC2$Data, classes=PCAinQC2$classes, PQN=F, mv_impute = T,
                                           glogScaling = F, scale=T,
                                           labels="QC", qc_label = QCreportObject$QC_label,
                                           plotTitle = "PCA, blank and QC MV filtered")


  QCreportObject$plots$SBRSDbefore <- doRSDplot(RSD=PCAinF2$RSD, plotTitle = "RSD (%) per sample group, blank and QC MV filtered")


  SBcorrected <- doQCRLSC(Data=MV_filtered, order=QCreportObject$metaData$table$injection_order,
                        batch=QCreportObject$metaData$table$batch, classes=class,
                        loessSpan=0, minQC = 5)

  PCAinSB <- prepareData(Data=SBcorrected, classes=class, blank = QCreportObject$Blank_label,
                       PQN=T, mv_impute = T, glogScaling = T,
                       qc_label = QCreportObject$QC_label, ignorelabel = "Removed")

  PCAinSBQC <- prepareData(Data=SBcorrected, classes=samp_lab5, blank = QCreportObject$Blank_label,
                       PQN=F, mv_impute = T, glogScaling = F,
                       qc_label = NULL, ignorelabel = "Removed")

  QCreportObject$plots$SBPCAafter <- doPCA (Data=PCAinSB$Data, classes=PCAinSB$classes, PQN=T, mv_impute = T,
                                          glogScaling = T, scale=F,
       labels="QC", qc_label = QCreportObject$QC_label, plotTitle = "PCA, blank and QC MV filtered, S/B corrected")

  QCreportObject$plots$SBPCAafterQC <- doPCA (Data=PCAinSBQC$Data, classes=PCAinSBQC$classes, PQN=F, mv_impute = T,
                                       glogScaling = F, scale= T,
                                  labels="QC", qc_label = QCreportObject$QC_label, plotTitle = "PCA, blank and QC MV filtered, S/B corrected")


  QCreportObject$plots$SBRSDafter <- doRSDplot(RSD=PCAinSB$RSD,
                          plotTitle = "RSD% per sample group, blank and QC MV filtered, S/B corrected")

  QCreportObject$tables$SBtableBefore <- doRSDtable(RSD = PCAinF2$RSD,
                                                  QC_label = QCreportObject$QC_label,
                                                  Blank_label = QCreportObject$Blank_label)

  QCreportObject$tables$SBtableAfter  <- doRSDtable(RSD = PCAinSB$RSD,
                                                  QC_label = QCreportObject$QC_label,
                                                  Blank_label = QCreportObject$Blank_label)

  addWorksheet (wb,"dataMatrix_corrected")
  writeData (wb,"dataMatrix_corrected", SBcorrected, rowNames = T)

  saveWorkbook (wb, QCreportObject$xlsxout, overwrite = T)

  QCreportObject
}
