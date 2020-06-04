#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_colour_manual
#'
NULL

#' Create QCreport data object to asses RSD
#'
#' @param QCreportObject Qcreport object

RSDstatistics <- function(QCreportObject) {

  QCreportObject$plots$RSDplot1 <- do_variability_plot(
    list_object=QCreportObject$data$PCAinF$RSD,
    plotTitle="RSD % per sample group")

  QCreportObject$tables$RSDtable1 <- do_variability_table(
    list_object=QCreportObject$data$PCAinF$RSD,
    QC_label=QCreportObject$QC_label,
    Blank_label=QCreportObject$Blank_label)

  QChit <- which(names(QCreportObject$data$PCAinF$RSD) ==
    QCreportObject$QC_label)

  if (length(QChit) > 0L) {
    RSDQC <- QCreportObject$data$PCAinF$RSD[[QChit]]
  } else {
    RSDQC <- rep(0L, nrow(QCreportObject$peakMatrix))
  }

  if (length(QCreportObject$QC_hits) > 0L) {
    RSDSample <- do_variability_list(
      peak_data=QCreportObject$peakMatrix[, -c(QCreportObject$QC_hits)],
      classes=rep("Sample",
      length(QCreportObject$metaData$samp_lab[-c(QCreportObject$QC_hits)])),
      method="RSD")
  } else {
    RSDSample <- do_variability_list(
      peak_data=QCreportObject$peakMatrix, classes=rep("Sample",
      length(QCreportObject$metaData$samp_lab)), method="RSD")
  }

  hits <- which(RSDQC <= 30L)

  clength <- length(hits)

  pcols <- c(rep("Sample", clength), rep("QC", clength))
  pcols <- factor(pcols, levels=c("Sample", "QC"), ordered=TRUE)

  A <- data.frame(x=c(1L:clength), RSD=c(RSDSample[[1L]][hits], RSDQC[hits]),
    class=pcols)

  if (!is.null(QCreportObject$QC_label)) {
    subt2 <- paste0("RSD% for QC and biological samples, ",
      round(length(hits) / length(RSDQC) * 100.0, 0L), "% (", length(hits), "/",
      length(RSDQC), ") < 30%")
  } else {
    subt2 <- paste0("RSD% of ", length(RSDQC), " features")
  }

  QCreportObject$plots$RSDplot2 <- ggplot(data=A,
    aes_(x=~x, y=~RSD, color=~class, shape=NA)) +
    geom_line() + xlab("Index") + ylab("RSD (%)") +
    scale_colour_manual(values=c("#fb9a99", "#00000090")) +
    theme_Publication(base_size=12L) +
    ggtitle(subt2)

  QCreportObject
}
