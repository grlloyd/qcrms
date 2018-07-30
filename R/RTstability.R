
#' Exctract and return ggplot2 object of EICs for specified xmcm features
#'
#' @param indexes vector of feautre indices to extract
#' @param rawfiles List of MsnBase packages raw file objects
#' @param class Sample class labels
#' @param xset XCMS object from withc to extrat chromatograms
#' @param Blank_labes Label used for Blank samples
#' @param QC_label Label used for QC samples
#' @export

RTstabilityAssement <- function ()

{
  rtmin = groupval(object=QCreportObject$xset, method = "medret", value = "rtmin", intensity = "into")
  rtmax = groupval(object=QCreportObject$xset, method = "medret", value = "rtmax", intensity = "into")
  rt = groupval(object=QCreportObject$xset, method = "medret", value = "rt", intensity = "into")

  QCreportObject$QC_hits

  rt <- rt[,QCreportObject$QC_hits]

  madrtQC <- apply (rt, 1, mad, constant=1)

  rt <- melt()
}
