#' @importFrom MSnbase readMSData
#' @importFrom gridExtra marrangeGrob
#' @importFrom MSnbase pData
#'
NULL

#' Create plots for EICs
#'
#' @param QCreportObject Qcreport object

EICs <- function(QCreportObject) {
  # Exclude QC and Blank samples from max intensity calculation, as these
  # can be very noisy signals
  maxints <- apply(QCreportObject$peakMatrix[,
    -c(QCreportObject$QC_hits, QCreportObject$Blank_hits)], 1L, max, na.rm=TRUE)

  if (is(QCreportObject$xset, "xcmsSet")){
      xcms_groups <- groupval(QCreportObject$xset)
      phenoData <- QCreportObject$xset@phenoData
  } else if (is(QCreportObject$xset, "XCMSnExp")){
      xcms_groups <- xcms::featureDefinitions(QCreportObject$xset)
      xcms_groups <- xcms_groups[, !colnames(xcms_groups) %in%
          c("peakidx", "ms_level")]
      phenoData <- MSnbase::pData(QCreportObject$xset)
  }
  
  # How many samples in total present per class
  gColn <- ncol(xcms_groups)
  totalCount <-
    apply(cbind(xcms_groups[, 8L:gColn], NULL), 1L, sum)

  # Check only these peakSets which are detected in at least 90% of samples
  ghits <- which(totalCount/nrow(phenoData)>=0.9)

  maxints[-c(ghits)] <- NA

  # Remove peaks eluating before 90s
  RThits <- which(xcms_groups[, 5L] < 90.0)

  if (length(RThits) > 0L) maxints[RThits] <- NA

  # To avoid plotting fragments, isotopes etc from one single metaboilite.
  # Randomly select 10 peak sets from most intense and present in 90% of samples

  inthits <- order(maxints, decreasing=TRUE)[seq_len(200L)]
  inthits <- inthits[sample(seq_len(200L), 10L)]

  # Check if selected indices are within range of extracted peak table,
  # replace 1 with 2. And te max peak nemubre with it's -1 value
  inthits[inthits == 1L] <- 2L
  inthits[inthits == nrow(QCreportObject$peakMatrix)] <-
    nrow(QCreportObject$peakMatrix) - 1L

  inthits2 <- NULL

  for (inth in seq_len(10L)) {
    inthits2 <- append(inthits2, c(inthits[inth], inthits[inth] - 1L,
      inthits[inth] + 1L))
  }

  # Using MSnBase package chromatograms function, will work only with
  # MSnBase 2.4.0 or later. Bioconductor 3.6
  rawf <- vector("list", length(QCreportObject$raw_paths))

  system.time({
    for (num in seq_len(length(rawf))) {
        cat("Reading data file: ", num, "\n")
        if (file.exists(QCreportObject$raw_paths[num])) {
          rawf[[num]] <- MSnbase::readMSData(QCreportObject$raw_paths[num],
            mode="onDisk")
        }
      }
    })

  #Class labels have to be reordereng according MS files in XCMS object
  class <-
    QCreportObject$metaData$samp_lab[order(order(QCreportObject$timestamps))]

  plots <- XCMS_EIC_plot(indexes=inthits2, rawfiles=rawf, class=class,
    xset=QCreportObject$xset)

  ml <- marrangeGrob(plots, nrow=3L, ncol=1L)
  ggsave(QCreportObject$pdfout, ml, width=20L, height=28L, units="cm")

  inthits <- order(maxints, decreasing=TRUE)[1L]
  inthits2 <- c(inthits, inthits - 1L, inthits + 1L)

  QCreportObject$plots$EICs  <- qcrms::XCMS_EIC_plot(indexes=inthits2[1L:3L],
    rawfiles=rawf, class=class,
    xset=QCreportObject$xset)

  QCreportObject
}
