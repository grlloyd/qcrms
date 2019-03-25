#' @import xcms
#' @importFrom MSnbase readMSData
#' @importFrom gridExtra marrangeGrob
#'
NULL

#' Create plots for EICs
#'
#' @param QCreportObject Qcreport object
#' @export


EICs <- function (QCreportObject)
{
  QCreportObject$xset@filepaths <- QCreportObject$raw_paths

  # Exclude QC and Blank samples from max intensity calculation, as these can be very noisy signals
  maxints <- apply(QCreportObject$peakMatrix[,-c(QCreportObject$QC_hits,QCreportObject$Blank_hits)],
                 1, max, na.rm=T)

  # How many samples in total present per class
  gColn <- ncol(QCreportObject$xset@groups)
  totalCount <- apply(cbind(QCreportObject$xset@groups[,8:gColn],NULL), 1, sum)

  # Check only these peakSets which are detected in at least 90% of samples
  ghits <- which(totalCount/nrow(QCreportObject$xset@phenoData)>=0.9)

  maxints[-c(ghits)] <- NA

  # Remove peaks eluating before 90s
  RThits <- which (QCreportObject$xset@groups[,5]<90)

  if (length(RThits)>0) maxints[RThits] <- NA

  # To avoid plotting fragments, isotopes etc from one single metaboilite. Randomly select 10 peak sets from most intense and present
  # in 90% of samples

  inthits <- order(maxints, decreasing = T)[1:200]
  inthits <- inthits[sample(1:200,10)]

  inthits2 <- NULL

  for (inth in 1:10)
  {
    inthits2 <- append(inthits2,c(inthits[inth], inthits[inth]-1, inthits[inth]+1))

  }

  # Using MSnBase package chromatograms function, will work only with MSnBase 2.4.0 or later. Bioconductor 3.6
  rawf <- vector("list", length(QCreportObject$xset@filepaths))

  system.time (
    { for (num in 1:length(rawf))
      {
        cat("Reading data file: ", num, "\n")
        if (file.exists(QCreportObject$xset@filepaths[num]))
        {
          rawf[[num]] <- MSnbase::readMSData(QCreportObject$xset@filepaths[num], mode="onDisk")
        }
      }
    })

  # PDF
  #pdf (paste(outputF,"QC_EICS.pdf",sep="/"))
  plots <- XCMS_EIC_plot(indexes = inthits2, rawfiles = rawf, class=QCreportObject$metaData$samp_lab, xset=QCreportObject$xset)

  ml <- marrangeGrob(plots, nrow=3, ncol=1)
  ggsave (QCreportObject$pdfout, ml, width=20, height=28, units="cm")

  inthits <- order(maxints, decreasing = T)[1]
  inthits2 <- c(inthits, inthits-1, inthits+1)

  QCreportObject$plots$EICs  <- qcrms::XCMS_EIC_plot(indexes = inthits2[1:3], rawfiles = rawf, class = QCreportObject$metaData$samp_lab,
                       xset=QCreportObject$xset)

  QCreportObject
}
