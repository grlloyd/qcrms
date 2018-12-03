#' @import ggplot2
#'
NULL

#' Create QCreport data object to asses RSD
#'
#' @param QCreportObject Qcreport object
#' @export

RSDstatistics <- function (QCreportObject)
{

  QCreportObject$plots$RSDplot1 <- doRSDplot(RSD=QCreportObject$data$PCAinF$RSD, plotTitle = "RSD % per sample group")

  QCreportObject$tables$RSDtable1 <- doRSDtable (RSD = QCreportObject$data$PCAinF$RSD,
                         QC_label = QCreportObject$QC_label,
                         Blank_label = QCreportObject$Blank_label)

  QChit <- which(names(QCreportObject$data$PCAinF$RSD)==QCreportObject$QC_label)

  if (length(QChit)>0) {
    RSDQC <- QCreportObject$data$PCAinF$RSD[[QChit]]
  } else {
    RSDQC <- rep(0, nrow(QCreportObject$peakMatrix))
  }

  if (length(QCreportObject$QC_hits)>0) {
    RSDSample <- unlist(doRSD(Data=QCreportObject$peakMatrix[,-c(QCreportObject$QC_hits)],
                            classes=rep("Sample",
                                        length(QCreportObject$metaData$samp_lab[-c(QCreportObject$QC_hits)]))))
  } else {
    RSDSample <- unlist(doRSD(Data=QCreportObject$peakMatrix, classes=rep("Sample",
                                                                       length(QCreportObject$metaData$samp_lab))))
  }

  hits <- which(RSDQC<=30)

  clength <- length(hits)

  pcols <- c(rep("Sample",clength), rep("QC",clength))
  pcols <- factor(pcols, levels=c("Sample", "QC"), ordered=T)


  A <- data.frame(x=c(1:clength), RSD=c(RSDSample[hits],RSDQC[hits]), class=pcols)

  if (!is.null(QCreportObject$QC_label)) {
    subt2 <- paste0 ("RSD% for QC and biological samples, ", round(length(hits)/length(RSDQC)*100,0),"% (", length(hits),"/",length(RSDQC),") < 30%")
  } else {
    subt2 <- paste0 ("RSD% of ", length(RSDQC)," features")
  }

  QCreportObject$plots$RSDplot2 <- ggplot (data=A, aes(x=x, y=RSD, color=class, shape=NA))+geom_line()+xlab("Index")+ylab("RSD (%)")+
    scale_colour_manual(values=c("#fb9a99", "#00000090"))+ theme_Publication(base_size = 12)+
    ggtitle(subt2)

  QCreportObject
}
