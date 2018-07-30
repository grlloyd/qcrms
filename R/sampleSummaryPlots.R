#' @import ggplot2
#' @import pmp
#'
NULL

#' Create plots of TIC chromatograms and general sample summary.
#'
#' @param QCreportObject Qcreport object
#' @export

sampleSummaryPlots <- function(QCreportObject)
{
  # Keep QC always the first, blank the second and all other classes alphabbetically
  QCreportObject$plotClass <- createClassAndColors(class=QCreportObject$samp.sum$Class,
                                                 QC_label = QCreportObject$QC_label,
                                                 Blank_label = QCreportObject$Blank_label)


  A <-data.frame (TICe = QCreportObject$TICs[order(QCreportObject$timestamps)],
                TICr = QCreportObject$TICraw[order(QCreportObject$timestamps)],
                nPeak = QCreportObject$samp.sum[,4],
                class=QCreportObject$plotClass$class,
                label=c(1:nrow(QCreportObject$samp.sum)),
                sample=QCreportObject$metaData$metaData$Sample[order(QCreportObject$timestamps)])


  QCreportObject$plots$ticplot_1 <- ggplot (data=A, aes(x=label, y=TICr, color=class, label=sample))+
    geom_line (mapping=aes(x=label,y=TICr), colour="#C0C0C0")+geom_point()+
    scale_colour_manual(values=QCreportObject$plotClass$manual_colors)+ theme_Publication(base_size = 12)+
    xlab("Injection order")+ylab("Total ion intensity of raw data")

  QCreportObject$plots$ticplot_2 <- ggplot (data=A, aes(x=label, y=TICe, color=class, label=sample))+
    geom_line (mapping=aes(x=label,y=TICe), colour="#C0C0C0")+geom_point()+
    scale_colour_manual(values=QCreportObject$plotClass$manual_colors)+ theme_Publication(base_size = 12)+
    xlab("Injection order")+ylab("Total ion intensity of extracted features")


  # Remove blank sample from correlation plots and table
  bhits <- which(QCreportObject$plotClass$class==QCreportObject$Blank_label)

  if (length(bhits)>0)
  {
    A <- A[-c(bhits),]
    QCreportObject$plotClass$manual_colors2 <- QCreportObject$plotClass$manual_colors[-2]
  } else
  {
    QCreportObject$plotClass$manual_colors2 <- QCreportObject$plotClass$manual_colors
  }

  QCreportObject$tables$corrMatrix <- matrix (nrow=3, ncol=2)

  corTICr <- cor.test (A[,2], 1:nrow(A))
  corTICe <- cor.test (A[,1], 1:nrow(A))
  corTICrTICe <- cor.test (A[,2], A[,1])

  QCreportObject$tables$corrMatrix[1,] <- c(round(corTICr$estimate,2), corTICr$p.value)
  QCreportObject$tables$corrMatrix[2,] <- c(round(corTICe$estimate,2), corTICe$p.value)
  QCreportObject$tables$corrMatrix[3,] <- c(round(corTICrTICe$estimate,2), corTICrTICe$p.value)

  QCreportObject$tables$corrMatrix <- data.frame(QCreportObject$tables$corrMatrix)
  rownames(QCreportObject$tables$corrMatrix) <- c("TIC raw vs injection order", "TIC extracted vs injection order", "TIC raw vs TIC extracted")
  colnames(QCreportObject$tables$corrMatrix) <- c("Pearson's product-moment correlation", "p-value")

  QCreportObject$plots$ticplot_3 <- ggplot (data=A, aes(x=TICr, y=TICe, color=class, label=label, shape=NA))+
  geom_text(na.rm=T)+
  xlab ("TIC (raw)") + ylab ("TIC (extracted)")+
  scale_colour_manual(values=QCreportObject$plotClass$manual_colors2)+ theme_Publication(base_size = 12)

  QCreportObject$plots$ticplot_4 <- ggplot (data=A, aes(x=nPeak, y=TICe, color=class, label=label, shape=NA))+
  geom_line()+ geom_text(na.rm=T)+
  xlab ("Number of detected features") + ylab ("TIC (extracted)")+
  scale_colour_manual(values=QCreportObject$plotClass$manual_colors2)+ theme_Publication(base_size = 12)

  QCreportObject
}
