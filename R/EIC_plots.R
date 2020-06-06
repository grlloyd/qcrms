#' @importFrom xcms groupval
#' @importFrom xcms featureValues
#' @importFrom xcms adjustedRtime
#' @importFrom xcms hasAdjustedRtime
#' @importFrom ggplot2 element_blank
#' @importFrom magrittr %>%
#'
NULL

#' Check if retention time correction was applied during xcms processing.
#'
#' @param xset XCMS object from which to extrat chromatograms.
#' @return logicla(1), TRUE if retention time correction has been applied.
#' @noRd

check_rt_correction <- function(xset){
    rt_corrected <- FALSE
    if (is(xset, "XCMSnExp")){
        rt_corrected <- xcms::hasAdjustedRtime(xset)
    }
    if (is (xset, "xcmsSet")){
        if (!all(unlist(xset@rt$raw) == unlist(xset@rt$corrected))){
            rt_corrected <- TRUE
        }
    }
    rt_corrected
}

#' For retention time corrected data return matching RT before correction
#' 
#' @param xset XCMS object from which to extrat chromatograms.
#' @param RTmi numeric(1), minimum retention time for the EIC.
#' @param RTmax numeric(1), maximum retention time tof the EIC.
#' @param chrn integer(1), index of the raw data file.
#' @noRd

.rt_corrected_extract_raw_rt <- function(xset, RTmi, RTma, chrn){
    RTind <- c(NA, NA)
    if (is(xset, "xcmsSet")){
        RTind[1L] <- which(xset@rt$corrected[[chrn]] >= RTmi)[1L]
        RTind[2L] <- which(xset@rt$corrected[[chrn]] >= RTma)[1L]
        RTmi <- xset@rt$raw[[chrn]][RTind[1L]]
        RTma <- xset@rt$raw[[chrn]][RTind[2L]]
    }
    if (is(xset, "XCMSnExp")){
        RTind[1L] <- which(xcms::adjustedRtime(xset,
            bySample=TRUE)[[chrn]] >= RTmi)[1L]
        RTind[2L] <- which(xcms::adjustedRtime(xset,
            bySample=TRUE)[[chrn]] >= RTma)[1L]
        RTmi <- xcms::rtime(xset, bySample=TRUE,
            adjusted=FALSE)[[chrn]][RTind[1L]]
        RTmax <- xcms::rtime(xset, bySample=TRUE,
            adjusted=FALSE)[[chrn]][RTind[2L]]
    }
    c(RTmi, RTma)
}

#' In extracted EIC replace raw retention time with corrected value
#' 
#' @param xset XCMS object from which to extrat chromatograms.
#' @param chrn integer(1), index of the raw data file.
#' @param chromatogram data.frame(), eic chromatogram with mz, rt and
#' intensity values.
#' @noRd

.rt_corrected_update_eic_rt <- function(xset, chrn, chromatogram){
    if (is(xset, "xcmsSet")){
        RThits <- which(xset@rt$raw[[chrn]] %in% chromatogram$rt)
        if (length(RThits) > 0L & length(RThits) == 
            nrow(chromatogram)) {
            chromatogram$rt <- xset@rt$corrected[[chrn]][RThits]
        }
    }
    if (is(xset, "XCMSnExp")){
         RThits <- which(xcms::rtime(xset, bySample=TRUE,
            adjusted=FALSE)[[chrn]] %in% chromatogram$rt)
        if (length(RThits) > 0L & length(RThits) == 
            nrow(chromatogram)) {
            chromatogram$rt <- xcms::adjustedRtime(xset,
                bySample=TRUE)[[chrn]][RThits]
        }
    }
    chromatogram
}

#' Return summaries of XCMS grouping RT and mz values per sample.
#' 
#' @param xset XCMS object from which to extrat chromatograms.
#' @param indexes vector of feautre indices to extract
#' @noRd

xcms_groups_rt_mz_summary <- function(xset, indexes){
    groups_rt_mz_list <- list()
    if(is(xset, "xcmsSet")){
        # Actual min/max rt, mz and intensities for all samples
        rtmin <- xcms::groupval(object=xset, method="medret", value="rtmin",
            intensity="into")
        rtmax <- xcms::groupval(object=xset, method="medret", value="rtmax",
            intensity="into")
        mzmin <- xcms::groupval(object=xset, method="medret", value="mzmin",
            intensity="into")
        mzmax <- xcms::groupval(object=xset, method="medret", value="mzmax",
            intensity="into")
    }
    if(is(xset, "XCMSnExp")){
        rtmin <- xcms::featureValues(object=xset, method="medret",
            value="rtmin", intensity="into")
        rtmax <- xcms::featureValues(object=xset, method="medret",
            value="rtmax", intensity="into")
        mzmin <- xcms::featureValues(object=xset, method="medret",
            value="mzmin", intensity="into")
        mzmax <- xcms::featureValues(object=xset, method="medret",
            value="mzmax", intensity="into")
    }

    groups_rt_mz_list$min_rt <- rtmin[indexes, , drop=FALSE]
    groups_rt_mz_list$max_rt <- rtmax[indexes, , drop=FALSE]
    groups_rt_mz_list$min_mz <- mzmin[indexes, , drop=FALSE]
    groups_rt_mz_list$max_mz <- mzmax[indexes, , drop=FALSE]

    groups_rt_mz_list
}

#' Exctract and return ggplot2 object of EICs for specified xcms features
#'
#' @param indexes vector of feautre indices to extract
#' @param rawfiles List of MsnBase packages raw file objects
#' @param class Sample class labels
#' @param xset XCMS object from withc to extrat chromatograms
#' @param Blank_label Label used for Blank samples
#' @param QC_label Label used for QC samples
#' @export

XCMS_EIC_plot <- function(indexes, rawfiles, class, xset, Blank_label="Blank",
  QC_label="QC") {

  # Check if RT correction has been applied
  rt_corrected <- check_rt_correction(xset=xset)

  peaks_rt_mz_list <- xcms_groups_rt_mz_summary(xset = xset, indexes = indexes)

  chromPlots <- vector("list", length(indexes))

  for (chromind in seq_len(length(indexes))) {
    cat("EIC: ", chromind, " ... sample: ")
    chroms <- vector("list", length(rawfiles))

    for (chrn in seq_len(length(rawfiles))) {
      cat(chrn, " ")
      mzmi <- peaks_rt_mz_list$min_mz[chromind, chrn]
      mzma <- peaks_rt_mz_list$max_mz[chromind, chrn]
      RTmi <- peaks_rt_mz_list$min_rt[chromind, chrn]
      RTma <- peaks_rt_mz_list$max_rt[chromind, chrn]

      # Find index of corrected RT and replace RT with raw value
      if (rt_corrected == TRUE) {
        rt_corr <- .rt_corrected_extract_raw_rt(xset=xset, RTmi=RTmi,
            RTma=RTma, chrn=chrn)
        RTmi <- rt_corr[1]
        RTma <- rt_corr[2]
        rm (rt_corr)
      }

      if (!all(is.na(c(mzmi, mzma, RTmi, RTma))) &&
        (!is.null(rawfiles[[chrn]]))) {
            ch <- tryCatch(
              rawfiles[[chrn]] %>%
              xcms::filterRt(rt=c(RTmi, RTma)) %>%
              xcms::filterMz(mz=c(mzmi, mzma)) %>%
              as("data.frame"), 
             error=function(e) {return(NULL)}
            )
            if (!is.null(ch)){
                chroms[[chrn]] <- ch
                # remove file column
                chroms[[chrn]]$file <- NULL
                # Replace raw RT with corrected if RT correction was applied
                if (nrow(chroms[[chrn]]) != 0L && rt_corrected == TRUE) {
                    chroms[[chrn]] <- .rt_corrected_update_eic_rt(xset=xset, 
                        chrn=chrn, chromatogram=chroms[[chrn]])
                }
            } else {
                chroms[[chrn]] <-data.frame(rt=NA, mz=NA, i=NA)
            }
        # To keeps track on class and sample labels, we need to have empty
        # data frame, if there are no EIC's to extract
        if (nrow(chroms[[chrn]]) == 0L) chroms[[chrn]] <-
          data.frame(rt=NA, mz=NA, i=NA)
      } else {
        chroms[[chrn]] <- data.frame(rt=NA, mz=NA, i=NA)
      }
    }

    # Probably there is cleaner way how to do that.
    #Assign sample number and it's class
    for (chrn in seq_len(length(rawfiles))) {
      chroms[[chrn]]$Class <- class[chrn]
      chroms[[chrn]]$sample <- chrn
    }

    # GGplot2 input
    A <- do.call(rbind, chroms)

    ptitle <- paste("EICs, m/z ",
      round(stats::median(peaks_rt_mz_list$min_mz[chromind, ],
          na.rm=TRUE), 5L), "-",
      round(stats::median(peaks_rt_mz_list$max_mz[chromind, ],
          na.rm=TRUE), 5L), sep="")

    # Reorder factor levels again, so that colors are consistent
    plotCols <- createClassAndColors(class=A$Class, QC_label=QC_label,
      Blank_label=Blank_label)

    A$Class <- plotCols$class

    chromPlots[[chromind]] <- ggplot(data=A,
      aes_(x=~rt, y=~i, color=~Class, group=~sample)) +
      geom_line() + theme_Publication(base_size=12L) +
      scale_colour_manual(values=plotCols$manual_colors) +
      xlab("retention time, s") + ylab("intensity") +
      ggtitle(label=NULL, subtitle=ptitle) +
      theme(legend.position=c(0.8, 0.8), legend.direction="vertical") +
      theme(legend.title=element_blank())

  cat("\n")
  }
  chromPlots
}
