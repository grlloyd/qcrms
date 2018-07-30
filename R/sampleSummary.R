#' @import xcms
#' @import openxlsx
#' @importFrom dplyr left_join
#' @importFrom  mzR openMSfile
#' @importFrom  mzR tic
#' @importFrom  mzR close
#'
NULL

#' Exctract and return ggplot2 object of EICs for specified xmcm features
#'
#' @param QCreportObject Qcreport object
#' @export


sampleSummary <- function (QCreportObject)
{

  if (!is.null(QCreportObject$raw_path))
  {
    QCreportObject$raw_paths <- paste(QCreportObject$raw_path,"/",rownames(QCreportObject$xset@phenoData),".mzML",sep="")
    } else
  {
    QCreportObject$raw_paths <- QCreportObject$xset@filepaths
  }

  # TimeStamp from mzML file

  QCreportObject$timestamps <- rep(NA, length(QCreportObject$raw_paths))

  for (i in 1:length(QCreportObject$raw_paths))
  {
    QCreportObject$timestamps[i] <- qcrms::mzML.startTimeStamp(filename = QCreportObject$raw_paths[i])
  }

  if (!is.null(QCreportObject$metaData$file))
  {
    QCreportObject$metaData$metaData <- read.xlsx (xlsxFile = QCreportObject$metaData$file, sheet=1)
    xsetNames <- data.frame(Sample=rownames(QCreportObject$xset@phenoData))
    QCreportObject$metaData$metaData <- left_join(xsetNames,QCreportObject$metaData$metaData, by="Sample")

  } else
  {
    QCreportObject$metaData$metaData <- QCreportObject$xset@phenoData
    colnames(QCreportObject$metaData$metaData) <- QCreportObject$metaData$classColumn
    QCreportObject$metaData$metaData[,1] <- as.vector( QCreportObject$metaData$metaData[,1])
    QCreportObject$metaData$metaData$Sample <- rownames(QCreportObject$metaData$metaData)

    if (!is.null(QCreportObject$Blank_label))
      {
        bh <- grep(QCreportObject$Blank_label,  rownames(QCreportObject$xset@phenoData))
        if (length(bh)>0) QCreportObject$metaData$metaData[bh,1] <- QCreportObject$Blank_label
      }
    if (!is.null(QCreportObject$QC_label))
      { qh <- grep(QCreportObject$QC_label,  rownames(QCreportObject$xset@phenoData))
        if (length(qh)>0) QCreportObject$metaData$metaData[qh,1] <- QCreportObject$QC_label
      }
  }


  chit <- which(colnames(QCreportObject$metaData$metaData)==QCreportObject$metaData$classColumn)

  # Quick and dirty sample classes from file names
  QCreportObject$QC_hits <- which(QCreportObject$metaData$metaData[,chit]==QCreportObject$QC_label)
  QCreportObject$Blank_hits <- which(QCreportObject$metaData$metaData[,chit]==QCreportObject$Blank_label)

  QCreportObject$metaData$samp_lab <- QCreportObject$metaData$metaData[,chit]

  peakt <- QCreportObject$xset@peaks

  #TIC for extracted peaks
  QCreportObject$TICs <- tapply (peakt[,7], peakt[,11], FUN=sum)

  # TIC's for raw files
  # mzR is anout 10 times faster to read Raw data files than MSnbase implementation. So for tic needs I am using it.

  QCreportObject$TICraw <- rep(NA, length(QCreportObject$TICs))

  QCreportObject$TICdata <- vector("list", length(QCreportObject$TICs))

  for (sn in 1:length(QCreportObject$TICraw))
  {
    A <- mzR::openMSfile(QCreportObject$raw_paths[sn])
    tic <- mzR::tic(A)
    QCreportObject$TICraw[sn] <- sum(tic$TIC)
    QCreportObject$TICdata[[sn]] <- tic$TIC
    mzR::close(A)
    rm (A, tic)
  }

  QCreportObject$metaData$samp_lab <- as.factor (QCreportObject$metaData$samp_lab)

  QCreportObject$samp.sum <- data.frame (sample=row.names(QCreportObject$xset@phenoData), meas.time= QCreportObject$timestamps,
                                       class=QCreportObject$metaData$samp_lab,
                                       peaks.detected=as.vector(table(peakt[,11])))

  colnames (QCreportObject$samp.sum) <- c("Sample","Measurement time","Class","Number of peaks")

  QCreportObject$samp.sum <- QCreportObject$samp.sum[order(QCreportObject$timestamps),]
  row.names(QCreportObject$samp.sum) <- NULL

  rNames <- c("Number of samples:", "Sample classes:")
  cValues <-  c((nrow(QCreportObject$xset@phenoData)),
              paste(unique(QCreportObject$metaData$metaData[,chit]), collapse=", "))

  QCreportObject$projectHeader <- rbind(QCreportObject$projectHeader,c("",""),cbind (rNames, cValues))

  QCreportObject
}
