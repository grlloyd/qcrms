#'
NULL

#' Perform severl PCA models and create plots
#'
#' @param QCreportObject Qcreport object

PCA <- function(QCreportObject) {

  PCAin <- prepareData(Data=QCreportObject$peakMatrix,
    classes=QCreportObject$metaData$samp_lab, blank=QCreportObject$Blank_label,
    PQN=FALSE, mv_impute=TRUE, glogScaling=FALSE,
    qc_label=QCreportObject$QC_label, ignorelabel="Removed")

  if (!is.null(QCreportObject$QC_label)) {

    samp_lab2 <- as.character(QCreportObject$metaData$samp_lab)
    samp_lab2[-c(QCreportObject$QC_hits)] <- "Removed"
    PCAinQC <- prepareData(Data=QCreportObject$peakMatrix, classes=samp_lab2,
      blank=QCreportObject$Blank_label, PQN=FALSE, mv_impute=TRUE,
      glogScaling=FALSE, qc_label=NULL, ignorelabel="Removed")

    # Remove leading QC's

    # If excludeQC is numeric vector, filter QC's to remove from sample name
    Rem_QC <- NULL

    if (is.numeric(QCreportObject$excludeQC)) {

      # Only numbers from QC_names
      QC_names <- colnames(QCreportObject$peakMatrix)[QCreportObject$QC_hits]
      QC_names <- as.numeric(gsub(".*?([0-9]+).*", "\\1", QC_names))

      Rem_QC <- QCreportObject$QC_hits[which(QC_names %in%
        QCreportObject$excludeQC)]

    } else if (is.character(QCreportObject$excludeQC)) {

      column_present <- QCreportObject$excludeQC %in%
        colnames(QCreportObject$metaData$table)
      if (!column_present) stop("Sample column specified to identify leading
        QC samples isn't present in meta data file.")
      Rem_QC <- which(!is.na(QCreportObject$metaData$
        table[, QCreportObject$excludeQC]))
    }

    samp_lab3 <- as.character(QCreportObject$metaData$samp_lab)

    samp_lab3[Rem_QC] <- "Removed"

    PCAinF <- prepareData(Data=QCreportObject$peakMatrix, classes=samp_lab3,
      blank=QCreportObject$Blank_label, PQN=FALSE, mv_impute=TRUE,
      glogScaling=FALSE, qc_label=QCreportObject$QC_label,
      ignorelabel="Removed")

    samp_lab4 <- samp_lab3
    samp_lab4[-c(QCreportObject$QC_hits)] <- "Removed"
    PCAinQC2 <- prepareData(Data=QCreportObject$peakMatrix, classes=samp_lab4,
      blank=QCreportObject$Blank_label, PQN=FALSE, mv_impute=TRUE,
      glogScaling=FALSE, qc_label=NULL, ignorelabel="Removed")

    QCreportObject$metaData$samp_lab <- samp_lab3

  } else {
    PCAinF <- PCAin
  }

  QCreportObject$data$PCAinF <- PCAinF

  QCreportObject$plots$PCAallSamples <-
    doPCA(Data=PCAin$Data, classes=PCAin$classes, PQN=FALSE, mv_impute=TRUE,
      glogScaling=FALSE, scale=TRUE, labels="none",
      qc_label=QCreportObject$QC_label,
      plotTitle="PCA, all QC and biological samples)")

  if (!is.null(QCreportObject$QC_label)) {

    QCreportObject$plots$PCAQCsamples <-
      doPCA(Data=PCAinQC$Data, classes=PCAinQC$classes, PQN=FALSE,
        mv_impute=TRUE, glogScaling=FALSE, scale=TRUE, labels="QC",
        qc_label=QCreportObject$QC_label, plotTitle="PCA, QC samples")

    QCreportObject$plots$PCAQCleading <-
      doPCA(Data=PCAinQC2$Data, classes=PCAinQC2$classes, PQN=FALSE,
        mv_impute=TRUE, glogScaling=FALSE, scale=TRUE, labels="QC",
        qc_label=QCreportObject$QC_label,
        plotTitle="PCA, QC samples (lead QCs removed)")

    QCreportObject$plots$PCAallQCleading <-
      doPCA(Data=PCAinF$Data, classes=PCAinF$classes, PQN=FALSE, mv_impute=TRUE,
      glogScaling=FALSE, scale=TRUE, labels="none",
      qc_label=QCreportObject$QC_label,
      plotTitle="PCA, all QC and biological samples, lead QCs removed)")
  }

  QCreportObject
}
