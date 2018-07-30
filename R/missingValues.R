#' @import ggplot2
#'
NULL

#' Create QCreport data object to asses missing values
#'
#' @param QCreportObject Qcreport object
#' @export

missingValues <- function (QCreportObject)
{
  countna=function (x)
    return((sum(is.na(x))/length(x))*100)

  across_samples <- data.frame(x=apply(QCreportObject$groupvals,2,countna))
  across_features <- data.frame(x=apply(QCreportObject$groupvals,1,countna))

  QCreportObject$plots$MVplot1 <- ggplot (data=across_samples, aes(x)) + geom_histogram()+
    xlab ("missing values, %")+ ggtitle("Missing values per sample")+
    xlim (0,100)+
    scale_colour_Publication()+ theme_Publication(base_size = 12)

  QCreportObject$plots$MVplot2 <- ggplot (data=across_features, aes(x)) + geom_histogram()+
    xlab ("missing values, %")+ ggtitle("Missing values per feature")+
    xlim (0,100)+
    scale_colour_Publication()+ theme_Publication(base_size = 12)

  cl <- unique (QCreportObject$metaData$samp_lab)
  out_across_features <- vector("list",length(cl))
  names (out_across_features) <- cl

  for (slab in 1: length(cl))
  {
    out_across_features[[slab]] <- apply(cbind(QCreportObject$groupvals[,QCreportObject$metaData$samp_lab==cl[slab]],NULL),1,
                                       countna)/length(which(QCreportObject$metaData$samp_lab==cl[slab]))*100
  }


  across_features <- unlist(out_across_features)

  across_features_lab <- NULL
  for (rd in 1:length(out_across_features))
  {
    across_features_lab <- append(across_features_lab, rep(names(out_across_features)[rd],length(out_across_features[[rd]])))
  }

  # Keep QC always the first
  QCreportObject$plots$MVplotClassS <- createClassAndColors(class=QCreportObject$metaData$samp_lab)
  QCreportObject$plots$MVplotClassF <- createClassAndColors(class=across_features_lab)

  across_samples<- data.frame(x=QCreportObject$plots$MVplotClassS$class, class=across_samples$x)
  across_features <- data.frame (x=QCreportObject$plots$MVplotClassF$class, class=across_features)

  QCreportObject$plots$MVplot3 <- ggplot (data=across_samples, aes(x=x, y=class, color=x)) + geom_boxplot(show.legend = F)+
    ylab("missing values, %")+
    ylim (0,100)+ xlab("")+
    scale_colour_manual(values=QCreportObject$plots$MVplotClassS$manual_colors)+ theme_Publication(base_size = 12)+
    coord_flip()

  QCreportObject$plots$MVplot4 <- ggplot (data=across_features, aes(x=x, y=class, color=x)) + geom_boxplot(show.legend = F)+
    ylab("missing values, %")+
    xlab("")+ scale_colour_manual(values=QCreportObject$plots$MVplotClassF$manual_colors)+ theme_Publication(base_size = 12)+
    ylim (0,100)+
    coord_flip()


  QCreportObject
}
