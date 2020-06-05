#' @importFrom ggplot2 geom_point
#' @importFrom stats cor.test
#'
NULL

#' Create plots of TIC chromatograms and general sample summary.
#'
#' @param QCreportObject Qcreport object

sampleSummaryPlots <- function(QCreportObject) {
  # Keep QC always the first, blank the second and all other classes
  # alphabbetically
  QCreportObject$plotClass <-
    createClassAndColors(class=QCreportObject$samp.sum$Class,
    QC_label=QCreportObject$QC_label,
    Blank_label=QCreportObject$Blank_label)

  A <-data.frame(TICe=QCreportObject$TICs[order(QCreportObject$timestamps)],
                TICr=QCreportObject$TICraw[order(QCreportObject$timestamps)],
                nPeak=QCreportObject$samp.sum[, 4L],
                class=QCreportObject$plotClass$class,
                label=c(seq_len(nrow(QCreportObject$samp.sum))),
                sample=QCreportObject$metaData$table$Sample,
                file.size=QCreportObject$samp.sum$`mzML file size (MB)`)

  QCreportObject$plots$ticplot_1 <- ggplot2::ggplot(data=A,
    aes_(x=~label, y=~TICr, color=~class, label=~sample)) +
    geom_line(mapping=aes_(x=~label, y=~TICr), colour="#C0C0C0") +
    geom_point() +
    scale_colour_manual(values=QCreportObject$plotClass$manual_colors) +
    theme_Publication(base_size=12L) +
    xlab("Injection order") + ylab("Total ion intensity of raw data")

  QCreportObject$plots$ticplot_2 <- ggplot2::ggplot(data=A, aes_(x=~label,
      y=~TICe, color=~class, label=~sample)) +
    geom_line(mapping=aes_(x=~label, y=~TICe), colour="#C0C0C0") +
    geom_point() +
    scale_colour_manual(values=QCreportObject$plotClass$manual_colors) +
    theme_Publication(base_size=12L)+
    xlab("Injection order") + ylab("Total ion intensity of extracted features")


  # Remove blank sample from correlation plots and table
  bhits <- which(QCreportObject$plotClass$class == QCreportObject$Blank_label)

  if (length(bhits) > 0L) {
    A <- A[-c(bhits), ]
    b_col_hit <- which(levels(QCreportObject$plotClass$class) ==
      QCreportObject$Blank_label)
    QCreportObject$plotClass$manual_colors2 <-
      QCreportObject$plotClass$manual_colors[-b_col_hit]
  } else {
    QCreportObject$plotClass$manual_colors2 <- QCreportObject$
      plotClass$manual_colors
  }

  QCreportObject$tables$corrMatrix <- matrix(nrow=3L, ncol=2L)

  if (!all(is.na(A[, 2L]))) {
    corTICr <- cor.test(A[, 2L], seq_len(nrow(A)))
    corTICe <- cor.test(A[, 1L], seq_len(nrow(A)))
    corTICrTICe <- cor.test(A[, 2L], A[, 1L])
    QCreportObject$tables$corrMatrix[1L, ] <- c(round(corTICr$estimate, 2L),
      corTICr$p.value)
    QCreportObject$tables$corrMatrix[2L, ] <- c(round(corTICe$estimate, 2L),
      corTICe$p.value)
    QCreportObject$tables$corrMatrix[3L, ] <- c(round(corTICrTICe$estimate, 2L),
      corTICrTICe$p.value)

    QCreportObject$tables$corrMatrix <-
      data.frame(QCreportObject$tables$corrMatrix)
    rownames(QCreportObject$tables$corrMatrix) <-
      c("TIC raw vs injection order",
        "TIC extracted vs injection order",
        "TIC raw vs TIC extracted")
    colnames(QCreportObject$tables$corrMatrix) <-
      c("Pearson's product-moment correlation", "p-value")

    QCreportObject$plots$ticplot_3 <- ggplot2::ggplot(data=A,
      aes_(x=~TICr, y=~TICe, color=~class, label=~label, shape=NA)) +
    geom_text(na.rm=TRUE)+
    xlab("TIC (raw)") + ylab("TIC (extracted)") +
    scale_colour_manual(values=QCreportObject$plotClass$manual_colors2) +
    theme_Publication(base_size=12L)

    QCreportObject$plots$ticplot_4 <- ggplot2::ggplot(data=A,
      aes_(x=~nPeak, y=~TICe, color=~class, label=~sample, shape=NA)) +
    geom_point() + geom_text(na.rm=TRUE, size=2L) +
    xlab("Number of detected features") + ylab("TIC (extracted)") +
    scale_colour_manual(values=QCreportObject$plotClass$manual_colors2) +
    theme_Publication(base_size=12L)
  }

  if (!all(is.na(A$file.size))) {
    QCreportObject$plots$ticplot_5 <- ggplot2::ggplot(data=A,
      aes_(x=~nPeak, y=~file.size, color=~class, label=~sample, shape=NA)) +
    geom_point() + geom_text(na.rm=TRUE, size=2L) +
    xlab("Number of detected features") + ylab("mzML file size (MB)") +
    scale_colour_manual(values=QCreportObject$plotClass$manual_colors2) +
    theme_Publication(base_size=12L)
  }

  QCreportObject
}
