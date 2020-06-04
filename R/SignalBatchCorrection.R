#' @importFrom pmp QCRSC
#' @importFrom pmp filter_samples_by_mv
#' @importFrom pmp filter_peaks_by_fraction
#' @importFrom pmp filter_peaks_by_blank
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#' @importFrom openxlsx saveWorkbook
#'
NULL

#' Apply signal batch correction
#'
#' @param QCreportObject Qcreport object

SignalBatchCorrection <- function(QCreportObject) {
  wb <- openxlsx::loadWorkbook(file=QCreportObject$xlsxout)

  metaData <- QCreportObject$metaData$table
  class <- metaData[, which(colnames(metaData) ==
    QCreportObject$metaData$classColumn)]

  QCreportObject$filtering$table <-
    data.frame(Filter=c("Before filtering", "Blank, fold_change=20, fraction=0",
    "MV Sample, max_perc_mv=0.5", "Features, method=QC, fraction=0.9",
    paste0("Featrues, method=", QCreportObject$filtering$mv_filter_method,
      ", fraction=", QCreportObject$filtering$mv_filter_frac)),
    "Number of features"=c(nrow(QCreportObject$peakMatrix)),
    "Number of samples"=c(ncol(QCreportObject$peakMatrix)),
    Applied=rep(TRUE, 5L),
    check.names=FALSE, stringsAsFactors=FALSE)

  # Blank filter
  if (QCreportObject$Blank_label %in% class) {
    blank_filtered <-
      pmp::filter_peaks_by_blank(QCreportObject$peakMatrix, 20L, class,
        blank_label=QCreportObject$Blank_label, qc_label=NULL,
        remove_samples=FALSE, remove_peaks=TRUE)
    QCreportObject$filtering$table$`Number of features`[2L:5L] <-
      nrow(blank_filtered)
  } else {
    blank_filtered <- QCreportObject$peakMatrix
    QCreportObject$filtering$table$Applied[2L] <- FALSE
  }

  # MV filter for all samples
  sample_filtered <- pmp::filter_samples_by_mv(df=blank_filtered, max_perc_mv=0.5,
    classes=class)
  flags <- attributes(sample_filtered)$flags
  if (any(flags[, "filter_samples_by_mv_flags"] == 0L)) {
    class <- class[as.logical(flags[, "filter_samples_by_mv_flags"])]
    metaData <- metaData[as.logical(flags[, "filter_samples_by_mv_flags"]), ]
    QCreportObject$filtering$table$`Number of samples`[3L:5L] <-
      ncol(sample_filtered)
    QCreportObject$filtering$samples_removed <-
      sort(rownames(flags)[flags[, "filter_samples_by_mv_flags"] == 0L])
  } else {
    sample_filtered <- blank_filtered
  }

  # QC MV fraction filter
  # MV in QC samples
  MV_filtered <- pmp::filter_peaks_by_fraction(sample_filtered, min_frac=0.9,
    classes=class, method="QC", qc_label=QCreportObject$QC_label)
  QCreportObject$filtering$table$`Number of features`[4L:5L] <-
    nrow(MV_filtered)

  # MV filter across all samples or within class
  MV_filtered <- pmp::filter_peaks_by_fraction(MV_filtered,
    min_frac=QCreportObject$filtering$mv_filter_frac,
    classes=class, method=QCreportObject$filtering$mv_filter_method,
    qc_label=NULL)

  QCreportObject$filtering$table$`Number of features`[5L] <- nrow(MV_filtered)

  PCAinF2 <- prepareData(Data=MV_filtered, classes=class,
    blank=QCreportObject$Blank_label, PQN=TRUE, mv_impute=TRUE,
    glogScaling=TRUE, qc_label=QCreportObject$QC_label, ignorelabel="Removed",
    store_lambda=TRUE)

  QCreportObject$filtering$glog_lambda_filtered <- PCAinF2$glog_lambda

  samp_lab5 <- class
  samp_lab5[-c(which(class==QCreportObject$QC_label))] <- "Removed"

  PCAinQC2 <- prepareData(Data=MV_filtered, classes=samp_lab5,
    blank=QCreportObject$Blank_label, PQN=FALSE, mv_impute=TRUE,
    glogScaling=FALSE, qc_label=NULL, ignorelabel="Removed")

  QCreportObject$plots$SBPCAbefore <- doPCA(Data=PCAinF2$Data,
    classes=PCAinF2$classes, PQN=TRUE, mv_impute=TRUE, glogScaling=TRUE,
    scale=FALSE, labels=QCreportObject$pca_scores_labels,
    qc_label=QCreportObject$QC_label, plotTitle="PCA, filtered")

  QCreportObject$plots$SBPCAbeforeQC <- doPCA(Data=PCAinQC2$Data,
    classes=PCAinQC2$classes, PQN=FALSE, mv_impute=TRUE, glogScaling=FALSE,
    scale=TRUE, labels="QC", qc_label=QCreportObject$QC_label,
    plotTitle="PCA, filtered")

  QCreportObject$plots$SBRSDbefore <-
    do_variability_plot(list_object=PCAinF2$RSD,
    plotTitle="RSD (%) per sample group, filtered")

  # If multiple batches are present create PCA plots per batch before correction
  nbatches <- unique(metaData$batch)

  if (length(nbatches) > 1L) {

    title_list <- classes_list <- data_list <- vector("list", length(nbatches))

    for (batch in seq_len(length(nbatches))) {

      batch_hits <- which(metaData$batch == nbatches[batch])
      data_list[[batch]] <- MV_filtered[, batch_hits]
      classes_list[[batch]] <- class[batch_hits]
      title_list[[batch]] <- paste("PCA, filtered,", "Batch", nbatches[batch])
    }

    title_list <- unlist(title_list)

    plots_per_batch <- doSummaryPlot(Data=data_list,
      classes=classes_list,
      plotTitle=title_list,
      PQN=TRUE,
      glogScaling=TRUE, mv_impute=TRUE, scale=FALSE, plot=FALSE, labels="all",
      blank=QCreportObject$Blank_label, qc_label=QCreportObject$QC_label)

    # pca plots are all odd numbers and RSD even numbers
    QCreportObject$plots$plots_per_batch_pca <-
      plots_per_batch[seq(1L, length(nbatches) * 2L, 2L)]
    QCreportObject$plots$plots_per_batch_rsd <-
      plots_per_batch[seq(2L, length(nbatches) * 2L, 2L)]

    # Create summary RSD plot for QC samples per batch
    qc_hits <- which(class==QCreportObject$QC_label)
    qc_data <- MV_filtered[, class==QCreportObject$QC_label]
    qc_class <- class[qc_hits]
    qc_batch <- metaData$batch[qc_hits]
    qc_class <- paste(qc_class, "B", qc_batch, sep="_")

    qc_batch_rsd <- do_variability_list(peak_data=qc_data,
      classes=qc_class, method="RSD")
    QCreportObject$plots$plots_per_batch_qc_rsd <-
      do_variability_plot(list_object=qc_batch_rsd,
      plotTitle="RSD (%) of QC samples per batch, filtered")
  }

  #S/B correction

  SBcorrected <- pmp::QCRSC(df=MV_filtered, order=metaData$injection_order,
    batch=metaData$batch, classes=class, spar=0.0, minQC=5L)

  PCAinSB <- prepareData(Data=SBcorrected, classes=class,
    blank=QCreportObject$Blank_label, PQN=TRUE, mv_impute=TRUE,
    glogScaling=TRUE, qc_label=QCreportObject$QC_label, ignorelabel="Removed",
    store_lambda=TRUE)

  QCreportObject$filtering$glog_lambda_filtered_SB <- PCAinSB$glog_lambda

  PCAinSBQC <- prepareData(Data=SBcorrected, classes=samp_lab5,
    blank=QCreportObject$Blank_label, PQN=FALSE, mv_impute=TRUE,
    glogScaling=FALSE, qc_label=NULL, ignorelabel="Removed")

  QCreportObject$plots$SBPCAfter <-
    doPCA(Data=PCAinSB$Data, classes=PCAinSB$classes, PQN=TRUE, mv_impute=TRUE,
    glogScaling=TRUE, scale=FALSE, labels="none",
    qc_label=QCreportObject$QC_label,
    plotTitle="PCA, filtered and S/B corrected")

  QCreportObject$plots$SBPCAfterQC <- doPCA(Data=PCAinSBQC$Data,
    classes=PCAinSBQC$classes, PQN=FALSE, mv_impute=TRUE, glogScaling=FALSE,
    scale=TRUE, labels="QC", qc_label=QCreportObject$QC_label,
    plotTitle="PCA, filtered and S/B corrected")

  QCreportObject$plots$SBRSDafter <-
    do_variability_plot(list_object=PCAinSB$RSD,
    plotTitle="RSD% per sample group, filtered and S/B corrected")

  QCreportObject$tables$SBtableBefore <-
    do_variability_table(list_object=PCAinF2$RSD,
    QC_label=QCreportObject$QC_label,
    Blank_label=QCreportObject$Blank_label)

  QCreportObject$tables$SBtableAfter <-
    do_variability_table(list_object=PCAinSB$RSD,
    QC_label=QCreportObject$QC_label,
    Blank_label=QCreportObject$Blank_label)

  openxlsx::addWorksheet(wb, "metaData_filtered")
  openxlsx::writeData(wb, "metaData_filtered", metaData, rowNames=TRUE)

  openxlsx::addWorksheet(wb, "dataMatrix_filtered")
  openxlsx::writeData(wb, "dataMatrix_filtered", MV_filtered, rowNames=TRUE)

  openxlsx::addWorksheet(wb, "dataMatrix_corrected")
  openxlsx::writeData(wb, "dataMatrix_corrected", SBcorrected, rowNames=TRUE)

  openxlsx::saveWorkbook(wb, QCreportObject$xlsxout, overwrite=TRUE)

  QCreportObject
}
