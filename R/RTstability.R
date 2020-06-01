#' @importFrom xcms groupval
#' @importFrom xcms featureValues
NULL

#' Several diagnostic tables and plot for retention time and m/z shift
#' assesment
#'
#' @param QCreportObject QC report object.

RTstabilityAssement <- function(QCreportObject){
  if (is(QCreportObject$xset, "xcmsSet")){
      rtmin <- xcms::groupval(object=QCreportObject$xset, method="medret",
          value="rtmin", intensity="into")
      rtmax <- xcms::groupval(object=QCreportObject$xset, method="medret",
          value="rtmax", intensity="into")
      rt <- xcms::groupval(object=QCreportObject$xset, method="medret",
          value ="rt", intensity="into")
      mz <- xcms::groupval(object=QCreportObject$xset, method="medret",
          value="mz", intensity="into")
  }
  else if (is(QCreportObject$xset, "XCMSnExp")){
      rtmin <- xcms::featureValues(object=QCreportObject$xset, method="medret",
          value="rtmin", intensity="into")
      rtmax <- xcms::featureValues(object=QCreportObject$xset, method="medret",
          value="rtmax", intensity="into")
      rt <- xcms::featureValues(object=QCreportObject$xset, method="medret",
          value ="rt", intensity="into")
      mz <- xcms::featureValues(object=QCreportObject$xset, method="medret",
          value="mz", intensity="into")
  }
  peak_width <- rtmax - rtmin

  # sample class labels are reordered according measurement order, so these
  # tables need to be reordered as well
  rt <- rt[, order(QCreportObject$timestamps)]
  peak_width <- peak_width[, order(QCreportObject$timestamps)]

  # Don't include data from "removed"(lead QC's and other samples specified for
  # removal in meta data) samples in the plots and tables
  if ("Removed" %in% unique(QCreportObject$metaData$samp_lab)) {
    rt <- rt[, !(QCreportObject$metaData$samp_lab == "Removed")]
    peak_width <- peak_width[, !(QCreportObject$metaData$samp_lab == "Removed")]
    samp_lab <- QCreportObject$metaData$samp_lab[!(QCreportObject$
      metaData$samp_lab == "Removed")]
  } else {
    samp_lab <- QCreportObject$metaData$samp_lab
  }

  #RSD vs MAD
  rt_rsd <- do_variability_list(peak_data=rt, classes=samp_lab, method="RSD")
  rt_mad <- do_variability_list(peak_data=rt, classes=samp_lab, method="MAD")

  QCreportObject$tables$RT_rsd <- do_variability_table(rt_rsd,
    QC_label=QCreportObject$QC_label, Blank_label=QCreportObject$Blank_label)
  QCreportObject$tables$RT_mad <- do_variability_table(rt_mad,
    QC_label=QCreportObject$QC_label, Blank_label=QCreportObject$Blank_label)

  QCreportObject$plots$MAD_rt <- do_variability_plot(rt_mad)

  # Peak width summary
  # Create a list of peak widths for each sample group, so it can be used by
  # do_variability_table and plot functions
  pw_median <- do_variability_list(peak_data=peak_width, classes=samp_lab,
    method="median")

  QCreportObject$tables$peak_width <- do_variability_table(pw_median,
    QC_label=QCreportObject$QC_label, Blank_label=QCreportObject$Blank_label)

  QCreportObject$plots$peak_width <- do_variability_plot(pw_median)
  QCreportObject$plots$peak_width$labels$y <-
    "Median of peak width per feature, s"

  # mz precision

  # Sample class labels are reordered according measurement order, so these
  # tables need to be reordered as well
  mz <- mz[, order(QCreportObject$timestamps)]

  mz_mean <- apply(mz, 1L, mean, na.rm=TRUE)

  mz_ppm <- (mz - mz_mean) / (mz_mean * 10.0^-6L)

  # Don't include data from "removed"(lead QC's and other samples specified for
  # removal in meta data) samples in the plots and tables
  if ("Removed" %in% unique(QCreportObject$metaData$samp_lab)) {
    mz_ppm <- mz_ppm[, !(QCreportObject$metaData$samp_lab == "Removed")]
  }

  mz_ppm_median_list <- do_variability_list(mz_ppm, classes=samp_lab,
    method="median")

  QCreportObject$tables$mz_median <- do_variability_table(mz_ppm_median_list,
    QC_label=QCreportObject$QC_label, Blank_label=QCreportObject$Blank_label)

  QCreportObject$plots$mz_median <- do_variability_plot(mz_ppm_median_list,
    ylim=c(-5L, 5L))
  QCreportObject$plots$mz_median$labels$y <- "median ppm of mz per feature"

  QCreportObject
}
