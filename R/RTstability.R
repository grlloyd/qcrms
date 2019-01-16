
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
  
  #RSD vs MAD
  rt_rsd <- do_variability_list(peak_data = rt, classes = QCreportObject$metaData$samp_lab, method = "RSD")
  rt_mad <- do_variability_list(peak_data = rt, classes = QCreportObject$metaData$samp_lab, method = "MAD")
  
  QCreportObject$tables$RT_rsd <- do_variability_table(rt_rsd, QC_label = QCreportObject$QC_label, Blank_label = QCreportObject$Blank_label)
  QCreportObject$tables$RT_mad <-do_variability_table(rt_mad, QC_label = QCreportObject$QC_label, Blank_label = QCreportObject$Blank_label)
  
  QCreportObject$plots$MAD_rt <- do_variability_plot(rt_mad)

  # Peak width summary
  # Create a list of peak widths for each sample group, so it can be used by do_variability_table and plot functions
  pw_median <- do_variability_list(peak_data = peak_width,classes = QCreportObject$metaData$samp_lab, method = "median")
  
  #pw_matrix <- do_variability_list(peak_data = peak_width,classes = QCreportObject$metaData$samp_lab, method = "none")
  
  QCreportObject$tables$peak_width <- do_variability_table(pw_median, QC_label = QCreportObject$QC_label, Blank_label = QCreportObject$Blank_label)
  #QCreportObject$tables$peak_width_all <- do_variability_table(pw_matrix)
  
  QCreportObject$plots$peak_width <- do_variability_plot(pw_median)
  QCreportObject$plots$peak_width$labels$y <- "Median of peak width per feature, s"
  
  #QCreportObject$plots$peak_width_all <- do_variability_plot(pw_matrix)
  #QCreportObject$plots$peak_width_all$labels$y <- "Peak width, s"
  
  # mz precision
  mz = xcms::groupval(object=QCreportObject$xset, method = "medret", value = "mz", intensity = "into")
  
  #sample class labels are reordered according measurement order, so these tables need to be reordered as well
  mz <- mz[, order(QCreportObject$timestamps)]
  
  mz_mean <- apply (mz, 1, mean, na.rm=T)

  mz_ppm <- abs((mz - mz_mean)/(mz_mean*10^-6))
  
  mz_ppm_median_list <- do_variability_list(mz_ppm, classes = QCreportObject$metaData$samp_lab, method = "median")
  #mz_ppm_list <- do_variability_list(mz_ppm, classes = QCreportObject$metaData$samp_lab, method = "none")
  
  QCreportObject$tables$mz_median <- do_variability_table(mz_ppm_median_list, QC_label = QCreportObject$QC_label, Blank_label = QCreportObject$Blank_label)
  #QCreportObject$tables$mz_all <- do_variability_table(mz_ppm_list, QC_label = QCreportObject$QC_label, Blank_label = QCreportObject$Blank_label)
  
  QCreportObject$plots$mz_median <- do_variability_plot(mz_ppm_median_list, ylim = 5)
  QCreportObject$plots$mz_median$labels$y <- "median ppm of mz per feature"
    
  #QCreportObject$plots$mz_all <- do_variability_plot(mz_ppm_list, ylim = 5)
  #QCreportObject$plots$mz_all$labels$y <- "ppm of mz across all samples and features"
  
  QCreportObject
}
