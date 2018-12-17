
#' Exctract and return ggplot2 object of EICs for specified xmcm features
#'
#' @param indexes vector of feautre indices to extract
#' @param rawfiles List of MsnBase packages raw file objects
#' @param class Sample class labels
#' @param xset XCMS object from withc to extrat chromatograms
#' @param Blank_labes Label used for Blank samples
#' @param QC_label Label used for QC samples
#' @export

RTstabilityAssement <- function (QCreportObject){
  
  rtmin = xcms::groupval(object=QCreportObject$xset, method = "medret", value = "rtmin", intensity = "into")
  rtmax = xcms::groupval(object=QCreportObject$xset, method = "medret", value = "rtmax", intensity = "into")

  peak_width <- rtmax-rtmin
  
  rt = xcms::groupval(object=QCreportObject$xset, method = "medret", value = "rt", intensity = "into")

  #sample class labels are reordered according measurement order, so these tables need to be reordered as well
  rt <- rt[, order(QCreportObject$timestamps)]
  peak_width <- peak_width[, order(QCreportObject$timestamps)]
  
  # Don't include leading/removed QC samples
  QC_hits <- which (QCreportObject$metaData$samp_lab==QCreportObject$QC_label)

  # Don't include blanks and removed samples
  skip_samples <- which(QCreportObject$metaData$samp_lab==QCreportObject$Blank_label)
  skip_samples <- c(skip_samples, which(QCreportObject$metaData$samp_lab=="Removed"), QC_hits)
  
  rt_rsd <- pmp::doRSD(Data=rt, classes = QCreportObject$metaData$samp_lab)
  
  QCreportObject$tables$RT_rsd <- pmp::doRSDtable(rt_rsd)
  
  madrtQC <- apply (rt[, QC_hits], 1, mad, constant=1, na.rm=T)
  madrt <- apply (rt[,-c(skip_samples)], 1, mad, constant=1, na.rm=T)
  
  vals <- data.frame(MAD=madrtQC, Class="QC")
  vals <- rbind(vals, data.frame(MAD=madrt, Class="Samples"))
  
  QCreportObject$plots$MAD_rt <- ggplot(data=vals, aes(x=Class, y=MAD)) + geom_violin() + theme_bw()
  
  peak_width_rsd <- doRSD(Data=peak_width, classes = QCreportObject$metaData$samp_lab)
  
  QCreportObject$plots$peak_width_RSD <- pmp::doRSDplot(peak_width_rsd)
  QCreportObject$tables$peak_width <- pmp::doRSDtable(peak_width_rsd)
  
  
  # mz precision
  mz = xcms::groupval(object=QCreportObject$xset, method = "medret", value = "mz", intensity = "into")
  
  #sample class labels are reordered according measurement order, so these tables need to be reordered as well
  mz <- mz[, order(QCreportObject$timestamps)]
  
  mz_mean <- apply (mz[,QC_hits], 1, mean, na.rm=T)

  mz_ppm <- abs((mz[,QC_hits] - mz_mean)/(mz_mean*10^-6))
  
  vals <- reshape2::melt(mz_ppm, value.name="ppm")
  
  QCreportObject$plots$mz_1 <- ggplot (data = vals, aes(x=ppm)) + geom_histogram() + ggtitle("QC, all features") + theme_bw()
  
  vals <- data.frame(ppm=apply(mz_ppm, 1, median))
  
  QCreportObject$plots$mz_2 <- ggplot (data = vals, aes(x=ppm)) + geom_histogram() + ggtitle("QC, median ppm per feature") + theme_bw()
  
  # Analytical sample
  mz_mean <- apply (mz[,-c(skip_samples)], 1, mean, na.rm=T)
  
  mz_ppm <- abs((mz[,-c(skip_samples)] - mz_mean)/(mz_mean*10^-6))
  
  vals <- reshape2::melt(mz_ppm, value.name="ppm")
  
  QCreportObject$plots$mz_3 <- ggplot (data = vals, aes(x=ppm)) + geom_histogram() + ggtitle("Samples, all features") + theme_bw()
  
  vals <- data.frame(ppm=apply(mz_ppm, 1, median))
  
  QCreportObject$plots$mz_4 <- ggplot (data = vals, aes(x=ppm)) + geom_histogram() + ggtitle("Samples, median ppm per feature") + theme_bw()
  
  #gridExtra::grid.arrange(ncol=2, QCreportObject$plots$mz_1, QCreportObject$plots$mz_2, QCreportObject$plots$mz_3, QCreportObject$plots$mz_4)
  
  QCreportObject
}
