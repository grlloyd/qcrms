#' @import ggplot2
#'
NULL

#' Create QC quality plots
#'
#' @param QCreportObject Qcreport object
#' @export

QC <- function(QCreportObject)
{
  # Reorder by measurement order
  batch =1
  hits <- QCreportObject$QC_hits

  sub_times <- QCreportObject$timestamps[hits]

  vals <- QCreportObject$groupvals[,hits]

  vals <- vals[,order(sub_times)]

  vals <- t(vals)

  #vals[is.na(vals)] <- 0
  vals <- scale(vals, center=T, scale=T)

  vals <- utils::stack(as.data.frame(t(vals)))

  QCreportObject$plots$QCplot1 <- ggplot(vals)+geom_boxplot(aes(x=ind, y=values))+
    #ggtitle(paste("QC samples from batch: ", batch,sep=""))+
    ggtitle("QC samples")+
    xlab("Injection order")+ylab("Signal intensity scaled to UV")+theme_Publication(base_size = 12)+
    theme(axis.text.x=element_text(angle=90, hjust=1))

  vals <- t(QCreportObject$groupvals[,order(QCreportObject$timestamps)])
  vals <- scale (vals, center=T, scale=T)
  vals <- stack(as.data.frame(t(vals)))

  Class <- rep(NA, nrow(vals))

  xsetNames <- rownames(QCreportObject$xset@phenoData)

  #I am quite sure there is fater and easier way how to do it
  for (i in 1:length(xsetNames))
      {
          hits <- which(as.character(vals$ind)==xsetNames[i])
          Class[hits] <- as.character(QCreportObject$metaData$samp_lab[i])
      }

  QCreportObject$plots$QCplotClass <- createClassAndColors(class=Class)
  vals$Class <-  QCreportObject$plots$QCplotClass$class


  QCreportObject$plots$QCplot2 <- ggplot(vals)+geom_boxplot(aes(x=ind, y=values,
                                          fill=Class))+
    #ggtitle(paste("All samples from batch: ",batch,sep=""))+
    ggtitle("All samples from")+
    xlab("Injection order")+ylab("Signal intensity scaled to UV")+theme_Publication(base_size = 12)+
    scale_x_discrete(breaks=NULL)+ scale_fill_manual(values= QCreportObject$plots$QCplotClass$manual_colors)

  QCreportObject
}
