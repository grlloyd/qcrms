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
  blank_filtered <- filter_peaks_by_blank(QCreportObject$peakMatrix, 20, class,
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
                labels=QCreportObject$pca_scores_labels, qc_label = QCreportObject$QC_label, plotTitle = "PCA, blank and QC MV filtered")


  QCreportObject$plots$SBPCAbeforeQC <- doPCA (Data=PCAinQC2$Data, classes=PCAinQC2$classes, PQN=F, mv_impute = T,
                                           glogScaling = F, scale=T,
                                           labels="QC", qc_label = QCreportObject$QC_label,
                                           plotTitle = "PCA, blank and QC MV filtered")


  QCreportObject$plots$SBRSDbefore <- do_variability_plot(list_object =PCAinF2$RSD, 
    plotTitle = "RSD (%) per sample group, blank and QC MV filtered")

  
  # If multiple batches are present create PCA plots per batch before correction
  nbatches <- unique(QCreportObject$metaD$table$batch)
    
  if (length (unique(QCreportObject$metaD$table$batch))>1){
    
    title_list <- classes_list <- data_list <- vector("list", length(nbatches))
    
    for (batch in 1:length(nbatches)){
      
      batch_hits <- which (QCreportObject$metaData$table$batch == nbatches[batch])
      data_list[[batch]] <- QCreportObject$peakMatrix[ , batch_hits]
      classes_list[[batch]] <- class[batch_hits]
      title_list[[batch]] <- paste ("PCA, blank and QC MV filtered,", "Batch", nbatches[batch])
    }
    
    title_list <- unlist (title_list)
    
    plots_per_batch <- doSummaryPlot(Data = data_list, 
                                          classes =classes_list, 
                                          plotTitle = title_list,
                                          PQN=T, 
                                          glogScaling=T, mv_impute=T, scale=F, plot = F, labels = "all", 
                                          blank=QCreportObject$Blank_label, qc_label = QCreportObject$QC_label)
    
    # pca plots are all odd numbers nad RSD even numbers
    QCreportObject$plots$plots_per_batch_pca <- plots_per_batch[seq(1, length(nbatches)*2, 2)] 
    QCreportObject$plots$plots_per_batch_rsd <- plots_per_batch[seq(2, length(nbatches)*2, 2)]   
    
    # Create summary RSD plot for QC samples per batch
    qc_hits <- which(class==QCreportObject$QC_label)
    qc_data <- QCreportObject$peakMatrix[, class==QCreportObject$QC_label]
    qc_class <- class[qc_hits]
    qc_batch <- QCreportObject$metaData$table$batch[qc_hits]
    qc_class <- paste(qc_class,"B",qc_batch, sep="_")
    
    qc_batch_rsd <- do_variability_list(peak_data = qc_data, classes = qc_class, method = "RSD")
    QCreportObject$plots$plots_per_batch_qc_rsd <- do_variability_plot (list_object = qc_batch_rsd, 
      plotTitle = "RSD (%) of QC samples per batch, blank and QC MV filtered")
  }
  
  #S/B correction

  SBcorrected <- sbcms::doQCRLSC(Data=MV_filtered, order=QCreportObject$metaData$table$injection_order,
                        batch=QCreportObject$metaData$table$batch, classes=class,
                        loessSpan=0, minQC = 5)
  
  # Temporary, to validate error from old pqn code
  #source("C:/Users/jankevia/Downloads/normalisation.R")
  
  #sub_hits <- which (class=="Removed")
  #sub_hits <- append (sub_hits, which(class=="Blank"))
  
  #class_sub <- class[-c(sub_hits)]
  
  #data_sub <- SBcorrected[,-c(sub_hits)]
  
  #wrong_pqn <- pqn_normalisation(df=t(data_sub), classes = class_sub, qc_label = "QC")
  
  #PCAinSB <- prepareData(Data=wrong_pqn$df, classes=class_sub, blank = "Blank",
  #                       PQN=F, mv_impute = T, glogScaling = T,
  #                       qc_label = QCreportObject$QC_label, ignorelabel = "Removed")

  #QCreportObject$plots$SBPCAfter_pqn_wrong <- doPCA (Data=PCAinSB$Data, classes=PCAinSB$classes, PQN=T, mv_impute = T,
  #                                         glogScaling = T, scale=F,
  #                                         labels="QC", qc_label = QCreportObject$QC_label, 
  #                                         plotTitle = "PCA, blank and QC MV filtered, S/B corrected")
  
    
  PCAinSB <- prepareData(Data=SBcorrected, classes=class, blank = QCreportObject$Blank_label,
                       PQN=T, mv_impute = T, glogScaling = T,
                       qc_label = QCreportObject$QC_label, ignorelabel = "Removed")

  PCAinSBQC <- prepareData(Data=SBcorrected, classes=samp_lab5, blank = QCreportObject$Blank_label,
                       PQN=F, mv_impute = T, glogScaling = F,
                       qc_label = NULL, ignorelabel = "Removed")

  QCreportObject$plots$SBPCAfter <- doPCA (Data=PCAinSB$Data, classes=PCAinSB$classes, PQN=T, mv_impute = T,
                                          glogScaling = T, scale=F,
       labels="none", qc_label = QCreportObject$QC_label, plotTitle = "PCA, blank and QC MV filtered, S/B corrected")

  QCreportObject$plots$SBPCAfterQC <- doPCA (Data=PCAinSBQC$Data, classes=PCAinSBQC$classes, PQN=F, mv_impute = T,
                                       glogScaling = F, scale= T,
                                  labels="QC", qc_label = QCreportObject$QC_label, plotTitle = "PCA, blank and QC MV filtered, S/B corrected")


  QCreportObject$plots$SBRSDafter <- do_variability_plot (list_object = PCAinSB$RSD,
                          plotTitle = "RSD% per sample group, blank and QC MV filtered, S/B corrected")

  QCreportObject$tables$SBtableBefore <- do_variability_table(list_object = PCAinF2$RSD,
                                                  QC_label = QCreportObject$QC_label,
                                                  Blank_label = QCreportObject$Blank_label)

  QCreportObject$tables$SBtableAfter  <- do_variability_table (list_object = PCAinSB$RSD,
                                                  QC_label = QCreportObject$QC_label,
                                                  Blank_label = QCreportObject$Blank_label)

  addWorksheet (wb,"dataMatrix_corrected")
  writeData (wb,"dataMatrix_corrected", SBcorrected, rowNames = T)

  saveWorkbook (wb, QCreportObject$xlsxout, overwrite = T)

  QCreportObject
}
