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

  metaData <- QCreportObject$metaData$table
  class <- metaData[,which(colnames(metaData) == QCreportObject$metaData$classColumn)]

  if (is.null(metaData$batch)){
    metaData$batch <- rep(1, length(class))
  }

  
  QCreportObject$filtering$table <- data.frame (Filter=c("Before filtering","Blank, fold_change=20, fraction=0",
    "MV Sample, max_perc_mv=0.5",
    "Features, method=QC, fraction=0.9",
    paste0("Featrues, method=", QCreportObject$filtering$mv_filter_method, 
      ", fraction=", QCreportObject$filtering$mv_filter_frac)),
    "Number of features"=c(nrow(QCreportObject$peakMatrix)),
    "Number of samples"=c(ncol(QCreportObject$peakMatrix)),
    Applied=rep(TRUE, 5),
    check.names=FALSE)
  
  # Blank filter
  if (QCreportObject$Blank_label %in% class){
    blank_filtered <- filter_peaks_by_blank(QCreportObject$peakMatrix, 20, class,
                                        blank_label=QCreportObject$Blank_label,
                                        qc_label = NULL, remove = FALSE)[[1]]
    QCreportObject$filtering$table$`Number of features`[2:5] <- nrow(blank_filtered)
  } else {
    blank_filtered <- QCreportObject$peakMatrix
    QCreportObject$filtering$table$Applied[2] <- FALSE
  }
  
  # MV filter for all samples
  sample_filtered <- filter_samples_by_mv(df=blank_filtered, max_perc_mv=0.5, classes = NULL)
  if (any(sample_filtered$flags[,2]==0)){
    class <- class[as.logical(sample_filtered$flags[,2])]
    metaData <- metaData[as.logical(sample_filtered$flags[,2]), ]
    QCreportObject$filtering$table$`Number of samples`[3:5] <- ncol(sample_filtered$df)
    QCreportObject$filtering$samples_removed <- sort(rownames(sample_filtered$flags)[sample_filtered$flags[,2]==0])
    sample_filtered <- sample_filtered$df
  } else {
    sample_filtered <- blank_filtered
  }
  
  # QC MV fraction filter
  # MV in QC samples
  MV_filtered <- filter_peaks_by_fraction(sample_filtered, min_frac = 0.9,
    classes=class, method = "QC", qc_label = QCreportObject$QC_label)[[1]]
  QCreportObject$filtering$table$`Number of features`[4:5] <- nrow(MV_filtered)

  # MV filter across all samples or within class
  MV_filtered <- filter_peaks_by_fraction(MV_filtered,
    min_frac = QCreportObject$filtering$mv_filter_frac,
    classes=class, method = QCreportObject$filtering$mv_filter_method,
    qc_label = NULL)[[1]]
  QCreportObject$filtering$table$`Number of features`[5] <- nrow(MV_filtered)
  
  PCAinF2 <- prepareData(Data=MV_filtered, classes=class,
                       blank = QCreportObject$Blank_label, PQN=T, mv_impute = T,
                       glogScaling = T,
                       qc_label = QCreportObject$QC_label, ignorelabel = "Removed",
                       store_lambda = TRUE)

  QCreportObject$filtering$glog_lambda_filtered <- PCAinF2$glog_lambda

  samp_lab5 <- class
  samp_lab5[-c(which(class==QCreportObject$QC_label))] <- "Removed"

  PCAinQC2 <- prepareData(Data=MV_filtered, classes=samp_lab5,
                        blank = QCreportObject$Blank_label, PQN=F, mv_impute = T,
                        glogScaling = F,
                        qc_label = NULL, ignorelabel = "Removed")


  QCreportObject$plots$SBPCAbefore <- doPCA (Data=PCAinF2$Data, classes=PCAinF2$classes, PQN=T, mv_impute = T,
                                       glogScaling = T, scale=F,
                labels=QCreportObject$pca_scores_labels, qc_label = QCreportObject$QC_label, plotTitle = "PCA, filtered")


  QCreportObject$plots$SBPCAbeforeQC <- doPCA (Data=PCAinQC2$Data, classes=PCAinQC2$classes, PQN=F, mv_impute = T,
                                           glogScaling = F, scale=T,
                                           labels="QC", qc_label = QCreportObject$QC_label,
                                           plotTitle = "PCA, filtered")


  QCreportObject$plots$SBRSDbefore <- do_variability_plot(list_object =PCAinF2$RSD,
    plotTitle = "RSD (%) per sample group, filtered")


  # If multiple batches are present create PCA plots per batch before correction
  nbatches <- unique(metaData$batch)

  if (length(nbatches) > 1){

    title_list <- classes_list <- data_list <- vector("list", length(nbatches))

    for (batch in 1:length(nbatches)){

      batch_hits <- which (metaData$batch == nbatches[batch])
      data_list[[batch]] <- MV_filtered[ , batch_hits]
      classes_list[[batch]] <- class[batch_hits]
      title_list[[batch]] <- paste ("PCA, filtered,", "Batch", nbatches[batch])
    }

    title_list <- unlist (title_list)

    plots_per_batch <- doSummaryPlot(Data = data_list,
      classes =classes_list,
      plotTitle = title_list,
      PQN=T,
      glogScaling=T, mv_impute=T, scale=F, plot = F, labels = "all",
      blank=QCreportObject$Blank_label, qc_label = QCreportObject$QC_label)

    # pca plots are all odd numbers and RSD even numbers
    QCreportObject$plots$plots_per_batch_pca <- plots_per_batch[seq(1, length(nbatches)*2, 2)]
    QCreportObject$plots$plots_per_batch_rsd <- plots_per_batch[seq(2, length(nbatches)*2, 2)]

    # Create summary RSD plot for QC samples per batch
    qc_hits <- which(class==QCreportObject$QC_label)
    qc_data <- MV_filtered[, class==QCreportObject$QC_label]
    qc_class <- class[qc_hits]
    qc_batch <- metaData$batch[qc_hits]
    qc_class <- paste(qc_class,"B",qc_batch, sep="_")

    qc_batch_rsd <- do_variability_list(peak_data = qc_data, classes = qc_class, method = "RSD")
    QCreportObject$plots$plots_per_batch_qc_rsd <- do_variability_plot (list_object = qc_batch_rsd,
      plotTitle = "RSD (%) of QC samples per batch, filtered")
  }

  #S/B correction

  SBcorrected <- sbcms::QCRSC(df=MV_filtered, order=metaData$injection_order,
                        batch=metaData$batch, classes=class,
                        spar=0, minQC = 5)
  

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
                       qc_label = QCreportObject$QC_label, ignorelabel = "Removed", store_lambda=TRUE)
  
  QCreportObject$filtering$glog_lambda_filtered_SB <- PCAinSB$glog_lambda

  PCAinSBQC <- prepareData(Data=SBcorrected, classes=samp_lab5, blank = QCreportObject$Blank_label,
                       PQN=F, mv_impute = T, glogScaling = F,
                       qc_label = NULL, ignorelabel = "Removed")

  QCreportObject$plots$SBPCAfter <- doPCA (Data=PCAinSB$Data, classes=PCAinSB$classes, PQN=T, mv_impute = T,
                                          glogScaling = T, scale=F,
       labels="none", qc_label = QCreportObject$QC_label, plotTitle = "PCA, filtered and S/B corrected")

  QCreportObject$plots$SBPCAfterQC <- doPCA (Data=PCAinSBQC$Data, classes=PCAinSBQC$classes, PQN=F, mv_impute = T,
                                       glogScaling = F, scale= T,
                                  labels="QC", qc_label = QCreportObject$QC_label, plotTitle = "PCA, filtered and S/B corrected")


  QCreportObject$plots$SBRSDafter <- do_variability_plot (list_object = PCAinSB$RSD,
                          plotTitle = "RSD% per sample group, filtered and S/B corrected")

  QCreportObject$tables$SBtableBefore <- do_variability_table(list_object = PCAinF2$RSD,
                                                  QC_label = QCreportObject$QC_label,
                                                  Blank_label = QCreportObject$Blank_label)

  QCreportObject$tables$SBtableAfter  <- do_variability_table (list_object = PCAinSB$RSD,
                                                  QC_label = QCreportObject$QC_label,
                                                  Blank_label = QCreportObject$Blank_label)

  addWorksheet (wb, "metaData_filtered")
  writeData (wb, "metaData_filtered", metaData, rowNames = T)
  
  addWorksheet (wb, "dataMatrix_filtered")
  writeData (wb, "dataMatrix_filtered", MV_filtered, rowNames = T)

  addWorksheet (wb, "dataMatrix_corrected")
  writeData (wb, "dataMatrix_corrected", SBcorrected, rowNames = T)
  
  saveWorkbook (wb, QCreportObject$xlsxout, overwrite = T)

  QCreportObject
}
