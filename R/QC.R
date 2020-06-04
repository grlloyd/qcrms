#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom reshape2 melt
#'
NULL

#' Prepare data for ggplot barplot
#'
#' @param vals Data frame to be processed
#' @param batch Vector with sample batch labels
#' @param class vector with sample clas labels

prepare_boxplot_multiple_batches <- function(vals, batch, class) {

  vals <- as.data.frame(vals)

  vals$batch <- batch

  vals$class <- class

  # keep order of the samples using ordered factor
  samp_order_batch <- rep(NA, nrow(vals))

  batch_name <- unique(batch)
  for (bnum in batch_name) {

    sub_hits <- which(vals$batch == bnum)
    samp_order_batch[sub_hits] <- seq_len(length(sub_hits))
  }

  samp_order_batch <-
    factor(samp_order_batch, ordered=TRUE, levels=unique(samp_order_batch))

  vals$sample <- factor(rownames(vals), ordered=TRUE, levels=rownames(vals))
  vals$sample_order <- samp_order_batch

  vals <-
    reshape2::melt(vals, id.vars=c("sample", "batch", "sample_order", "class"))

  vals
}

#' Create QC quality plots
#'
#' @param QCreportObject Qcreport object

QC <- function(QCreportObject) {

  batch <- QCreportObject$metaData$table$batch

  if (is.null(batch)) {
    QCreportObject$metaData$table$batch <- 1L
    batch <- QCreportObject$metaData$table$batch
  }

  hits <- QCreportObject$QC_hits

  vals <- QCreportObject$peakMatrix[, hits]

  batch <- batch[hits]
  class <-
    QCreportObject$metaData$table[hits, QCreportObject$metaData$classColumn]

  vals <- t(vals)
  vals <- scale(vals, center=TRUE, scale=TRUE)

  if (length(unique(batch)) > 1L) {

    vals <- prepare_boxplot_multiple_batches(vals=vals, batch=batch,
      class=class)

    QCreportObject$plots$QCplot1 <- ggplot(vals) +
      ggtitle("QC samples") +
      geom_hline(yintercept=c(-2L, 2L), color="red") +
      geom_boxplot(aes_(x=~sample_order, y=~value)) +
      xlab("Injection order")+ylab("Signal intensity scaled to UV") +
        theme_Publication(base_size=12L)+
      theme(axis.text.x=element_text(angle=90L, hjust=1L)) +
      facet_wrap(~ batch, ncol=1L)


  } else {

    vals <- utils::stack(as.data.frame(t(vals)))

    QCreportObject$plots$QCplot1 <- ggplot(vals) +
      geom_boxplot(aes_(x=~ind, y=~values)) +
      ggtitle("QC samples") +
      xlab("Injection order") + ylab("Signal intensity scaled to UV") +
        theme_Publication(base_size=12L) +
      theme(axis.text.x=element_text(angle=90L, hjust=1L))
  }

  # The same plot with all samples included
  batch <- QCreportObject$metaData$table$batch
  class <- QCreportObject$metaData$table[, QCreportObject$metaData$classColumn]

  vals <- t(QCreportObject$peakMatrix)
  vals <- scale(vals, center=TRUE, scale=TRUE)

  if (length(unique(batch)) > 1L) {

    vals <- prepare_boxplot_multiple_batches(vals=vals, batch=batch,
      class=class)

    QCreportObject$plots$QCplot2 <- ggplot(vals) +
      ggtitle("QC samples") +
      geom_hline(yintercept=c(-2L, 2L), color="red") +
      geom_boxplot(aes_(x=~sample_order, y=~value, fill=~class),
        outlier.shape=NA) +
      xlab("Injection order") + ylab("Signal intensity scaled to UV") +
      theme_Publication(base_size=12L)+
      theme(axis.text.x=element_text(angle=90L, hjust=1L)) +
      facet_wrap(~ batch, ncol=1L)

  } else {

    vals <- prepare_boxplot_multiple_batches(vals=vals, batch=batch,
       class=class)

    QCreportObject$plots$QCplotClass <- createClassAndColors(class=class)
    vals$Class <-  QCreportObject$plots$QCplotClass$class

    QCreportObject$plots$QCplot2 <- ggplot(vals)+geom_boxplot(aes_(x=~sample,
      y=~value, fill=~Class))+ ggtitle("All samples") +
      xlab("Injection order")+ylab("Signal intensity scaled to UV") +
      theme_Publication(base_size=12L) +
      scale_x_discrete(breaks=NULL) +
      scale_fill_manual(values= QCreportObject$plots$QCplotClass$manual_colors)

  }
  QCreportObject
}
