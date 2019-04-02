#' @importFrom  xcms groupval
#' @import ggplot2
#'
NULL

#' Exctract and return ggplot2 object of EICs for specified xcms features
#'
#' @param indexes vector of feautre indices to extract
#' @param rawfiles List of MsnBase packages raw file objects
#' @param class Sample class labels
#' @param xset XCMS object from withc to extrat chromatograms
#' @param Blank_label Label used for Blank samples
#' @param QC_label Label used for QC samples
#' @export

XCMS_EIC_plot <- function (indexes, rawfiles, class, xset, Blank_label="Blank", QC_label="QC")
{

  # Check if RT correction has been applied
  if (!all(unlist(xset@rt$raw)==unlist(xset@rt$corrected))) {
    RTcorrected = T
  } else {
    RTcorrected = F
  }

  # Actual min/max rt, mz and intensities for all samples
  rtmin = xcms::groupval(object=xset, method = "medret", value = "rtmin", intensity = "into")
  rtmax = xcms::groupval(object=xset, method = "medret", value = "rtmax", intensity = "into")

  min_rt = rbind(rtmin[indexes, ], NULL)
  max_rt = rbind(rtmax[indexes, ], NULL)

  mzmin = xcms::groupval(object=xset, method = "medret", value = "mzmin", intensity = "into")
  mzmax = xcms::groupval(object=xset, method = "medret", value = "mzmax", intensity = "into")

  min_mz = rbind(mzmin[indexes,], NULL)
  max_mz = rbind(mzmax[indexes,], NULL)

  chromPlots <- vector ("list", length(indexes))

  for (chromind in 1:length(indexes)) {
    cat ("EIC: ", chromind, " ... sample: ")
    chroms <- vector("list",length(rawfiles))

    for (chrn in 1:length(rawfiles)) {
      cat (chrn, " ")
      mzmi <- min_mz[chromind, chrn]
      mzma <- max_mz[chromind, chrn]
      RTmi <- min_rt[chromind, chrn]
      RTma <- max_rt[chromind, chrn]

      if (RTcorrected==T) {
        RTind <- c(NA,NA)
        RTind[1] <- which(xset@rt$corrected[[chrn]]>=RTmi)[1]
        RTind[2] <- which(xset@rt$corrected[[chrn]]>=RTma)[1]

        RTmi <- xset@rt$raw[[chrn]][RTind[1]]
        RTma <- xset@rt$raw[[chrn]][RTind[2]]
      }

      if (!all(is.na(c(mzmi,mzma,RTmi,RTma))) && (!is.null(rawfiles[[chrn]]))) {
        chroms[[chrn]] <- xcms::extractMsData (rawfiles[[chrn]], mz=c(mzmi, mzma), rt=c(RTmi,RTma), msLevel=1)[[1]]

        # Replace raw RT with corrected if RT correction was applied
        if (nrow(chroms[[chrn]])!=0 && RTcorrected==T) {
          RThits <- which(xset@rt$raw[[chrn]] %in% chroms[[chrn]]$rt)

          # For some weird reason, extracMsData can return mor than one peak for the same RT
          if (length(RThits)!=nrow(chroms[[chrn]])) {
              RThits <- rownames (chroms[[chrn]])
              RThits <- sub("F1.S0", "", RThits)
              RThits <- round(as.numeric(RThits),0)
          }

          chroms[[chrn]]$rt <- xset@rt$corrected[[chrn]][RThits]

        }

        # To keeps track on class and sample labels, we need to have empty data frame, if there are no EIC's to extract
        if (nrow(chroms[[chrn]])==0) chroms[[chrn]] <- data.frame(rt=NA, mz=NA, i=NA)
      } else {
        chroms[[chrn]] <- data.frame(rt=NA, mz=NA, i=NA)
      }
    }


    # Probably there is cleaner way how to do that. Assign sample number and it's class
    for (chrn in 1:length(rawfiles))
    {
      chroms[[chrn]]$Class <- class[chrn]
      chroms[[chrn]]$sample <- chrn
    }

    # GGplot2 input
    A <- do.call (rbind,chroms)

    ptitle <- paste("EICs, m/z ",round(stats::median(min_mz[chromind,], na.rm=T),5),"-",round(stats::median(max_mz[chromind,], na.rm=T),5),sep="")

    # Reorder factor levels again, so that colors are consistent
    plotCols <- createClassAndColors(class = A$Class, QC_label = QC_label, Blank_label = Blank_label)

    A$Class <- plotCols$class

    chromPlots[[chromind]] <- ggplot (data=A, aes(x=rt, y=i, color=Class, group=sample))+ geom_line()+ theme_Publication(base_size = 12)+
      scale_colour_manual(values=plotCols$manual_colors)+
      xlab("retention time, s")+ylab("intensity")+
      ggtitle(label=NULL, subtitle=ptitle)+
      theme(legend.position = c(0.8, 0.8), legend.direction = "vertical")+
      theme(legend.title=element_blank())

  cat ("\n")
  }
  chromPlots
}
