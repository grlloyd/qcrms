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

    if (!is.null(QCreportObject$xset)) {
        # Implementation of XProcessHistory class isn't really clear.
        # For now only peak picking parameters for CentWave are implemented
        if (is(QCreportObject$xset@.processHistory[[1L]]@param,
            "CentWaveParam")) {
            col1 <-  c("method", "ppm", "peakwidth", "mzdif", "snthresh",
            "integrate", "noise", "prefilter")
            col2 <- c(
                "CentWave",
                QCreportObject$xset@.processHistory[[1L]]@param@ppm,
                paste(QCreportObject$xset@.processHistory[[1L]]@param@peakwidth,
                    collapse="-"),
                QCreportObject$xset@.processHistory[[1L]]@param@mzdiff,
                QCreportObject$xset@.processHistory[[1L]]@param@snthresh,
                QCreportObject$xset@.processHistory[[1L]]@param@integrate,
                QCreportObject$xset@.processHistory[[1L]]@param@noise,
                paste(QCreportObject$xset@.processHistory[[1L]]@param@prefilter,
                    collapse=", ")
            )
            QCreportObject$peakPickingParams <-
                rbind(QCreportObject$peakPickingParams, cbind(col1, col2))
            colnames(QCreportObject$peakPickingParams) <- c("", "")
        }
    }
    QCreportObject
}
