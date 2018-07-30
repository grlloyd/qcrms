#' Create a header for QC report
#'
#' @param QCreportObject QCreportObject
#' @export

createProjectHeader <- function (QCreportObject)
{

  setwd (QCreportObject$projectdir)

  load (QCreportObject$FILENAME_RDATA)

  projdet <- unlist(strsplit(QCreportObject$projectdir,"/"))

  dat <- cbind(c("Creator:","Project reference:","Assay:","Sample type:")
             ,c(QCreportObject$projectInfo$author,
               QCreportObject$projectInfo$InternalProjectRef,
              QCreportObject$projectInfo$assay,
              QCreportObject$projectInfo$Dataset))

  colnames(dat) <- c("","")

  QCreportObject$projectHeader <- dat

  QCreportObject$xset <- xset

  if (exists ("listOFlistArguments"))
  {
    QCreportObject$listOFlistArguments <- listOFlistArguments
  }
  QCreportObject
}




