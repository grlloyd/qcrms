#' Create a header for QC report
#'
#' @param QCreportObject QCreportObject
#' @export

createProjectHeader <- function (QCreportObject)
{

  dat <- cbind(c("Creator:","Project reference:","Assay:","Sample type:"),
               c(QCreportObject$projectInfo$author,
               QCreportObject$projectInfo$InternalProjectRef,
               QCreportObject$projectInfo$assay,
               QCreportObject$projectInfo$Dataset))

  colnames(dat) <- c("","")

  QCreportObject$projectHeader <- dat

  QCreportObject$peakPickingParams <- c("Number of peak groups:", nrow(QCreportObject$peakMatrix))
  QCreportObject$peakPickingParams <- rbind(QCreportObject$peakPickingParams, NULL)
  
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




