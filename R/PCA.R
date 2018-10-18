#' @import xcms
#' @import ggplot2
#' @import pmp
#'
NULL

#' Perform severl PCA models and create plots
#'
#' @param QCreportObject Qcreport object
#' @export

PCA <- function (QCreportObject)
{

  QCreportObject$peakMatrix <- xcms::groupval (object=QCreportObject$xset, method="medret",value="into",intensity="into")


  PCAin <- pmp::prepareData(Data=QCreportObject$peakMatrix, classes=QCreportObject$metaData$samp_lab,
                     blank = QCreportObject$Blank_label, PQN=F, mv_impute = T, glogScaling = F,
                     qc_label = QCreportObject$QC_label, ignorelabel = "Removed")

  if (!is.null(QCreportObject$QC_label))
  {
    samp_lab2 <- as.character(QCreportObject$metaData$samp_lab)
    samp_lab2[-c(QCreportObject$QC_hits)] <- "Removed"
    PCAinQC <- pmp::prepareData(Data=QCreportObject$peakMatrix, classes=samp_lab2,
                         blank = QCreportObject$Blank_label, PQN=F, mv_impute = T,
                         glogScaling = F, qc_label = NULL, ignorelabel = "Removed")

    # Remove leading QC's
    QC_names <- rownames(QCreportObject$xset@phenoData)[QCreportObject$QC_hits]

    # Only numbers from QC_names
    QC_names <- as.numeric(gsub(".*?([0-9]+).*", "\\1", QC_names))

    Rem_QC <- QCreportObject$QC_hits[which(QC_names%in%QCreportObject$excludeQC)]

    samp_lab3 <- as.character(QCreportObject$metaData$samp_lab)
    samp_lab3[Rem_QC] <- "Removed"
    PCAinF <- pmp::prepareData(Data=QCreportObject$peakMatrix, classes=samp_lab3,
                        blank = QCreportObject$Blank_label, PQN=F, mv_impute = T,
                        glogScaling = F, qc_label = QCreportObject$QC_label, ignorelabel = "Removed")

    samp_lab4 <- samp_lab3
    samp_lab4[-c(QCreportObject$QC_hits)] <- "Removed"
    PCAinQC2 <- prepareData(Data=QCreportObject$peakMatrix, classes=samp_lab4,
                          blank = QCreportObject$Blank_label, PQN=F, mv_impute = T,
                          glogScaling = F, qc_label = NULL, ignorelabel = "Removed")


  } else
  {
    PCAinF <- PCAin
  }

  QCreportObject$data$PCAinF <- PCAinF

  QCreportObject$plots$PCAallSamples <- doPCA (Data=PCAin$Data, classes=PCAin$classes, PQN=F, mv_impute = T, glogScaling = F,
       scale=T, labels="QC", qc_label = QCreportObject$QC_label,
       plotTitle = "PCA, all QC and biological samples)")

  if (!is.null(QCreportObject$QC_label))
  {
    QCreportObject$plots$PCAQCsamples <- doPCA (Data=PCAinQC$Data, classes=PCAinQC$classes, PQN=F, mv_impute = T, glogScaling = F,
       scale=T, labels="QC", qc_label = QCreportObject$QC_label,
       plotTitle = "PCA, QC samples")

    QCreportObject$plots$PCAQCleading <- doPCA (Data=PCAinQC2$Data, classes=PCAinQC2$classes, PQN=F, mv_impute = T, glogScaling = F,
       scale=T, labels="QC", qc_label = QCreportObject$QC_label,
       plotTitle = "PCA, QC samples (lead QCs removed)")

    QCreportObject$plots$PCAallQCleading <- doPCA (Data=PCAinF$Data, classes=PCAinF$classes, PQN=F, mv_impute = T, glogScaling = F, scale=T,
       labels="QC", qc_label = QCreportObject$QC_label,
       plotTitle = "PCA, all QC and biological samples, lead QCs removed)")
  }


  QCreportObject$peakPickingParams <- c("Number of peak groups:", nrow(QCreportObject$peakMatrix))
  QCreportObject$peakPickingParams <- rbind (QCreportObject$peakPickingParams, NULL)

  if (!is.null(QCreportObject$listOFlistArguments))
  {
    col1 <- c("method","ppm","peakwidth","mzdif","snthresh","integrate","noise","prefilter")
    col2 <- c(QCreportObject$listOFlistArguments[[1]]$method,
              QCreportObject$listOFlistArguments[[1]]$ppm,
    paste(QCreportObject$listOFlistArguments[[1]]$peakwidth, collapse = "-"),
    QCreportObject$listOFlistArguments[[1]]$mzdiff,
    QCreportObject$listOFlistArguments[[1]]$snthresh,
    QCreportObject$listOFlistArguments[[1]]$integrate,
    QCreportObject$listOFlistArguments[[1]]$noise,
    paste(QCreportObject$listOFlistArguments[[1]]$prefilter,collapse=", "))

    QCreportObject$peakPickingParams <- rbind(QCreportObject$peakPickingParams,cbind(col1, col2))

  }

  colnames(QCreportObject$peakPickingParams) <- c("","")

  QCreportObject
}
