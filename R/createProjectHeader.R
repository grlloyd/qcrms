#' Create a header for QC report
#'
#' @param QCreportObject QCreportObject

createProjectHeader <- function(QCreportObject) {
    dat <- cbind(c("Creator:", "Project reference:", "Assay:", "Sample type:"),
        c(QCreportObject$projectInfo$author,
        QCreportObject$projectInfo$InternalProjectRef,
        QCreportObject$projectInfo$assay,
        QCreportObject$projectInfo$Dataset))

    colnames(dat) <- c("", "")

    QCreportObject$projectHeader <- dat

    QCreportObject$peakPickingParams <- c("Number of peak groups:",
        nrow(QCreportObject$peakMatrix))
    QCreportObject$peakPickingParams <-
        rbind(QCreportObject$peakPickingParams, NULL)

    if (!is.null(QCreportObject$listOFlistArguments)) {
        col1 <-  c("method", "ppm", "peakwidth", "mzdif", "snthresh",
            "integrate", "noise", "prefilter")
        col2 <- c(QCreportObject$listOFlistArguments[[1L]]$method,
            QCreportObject$listOFlistArguments[[1L]]$ppm,
            paste(QCreportObject$listOFlistArguments[[1L]]$peakwidth,
                collapse="-"),
            QCreportObject$listOFlistArguments[[1L]]$mzdiff,
            QCreportObject$listOFlistArguments[[1L]]$snthresh,
            QCreportObject$listOFlistArguments[[1L]]$integrate,
            QCreportObject$listOFlistArguments[[1L]]$noise,
            paste(QCreportObject$listOFlistArguments[[1L]]$prefilter,
                collapse=", "))

        QCreportObject$peakPickingParams <-
            rbind(QCreportObject$peakPickingParams, cbind(col1, col2))
    }
    colnames(QCreportObject$peakPickingParams) <- c("", "")
    QCreportObject
}
