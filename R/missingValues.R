#' @import ggplot2
#'
NULL

#' Create QCreport data object to asses missing values
#'
#' @param QCreportObject Qcreport object

missingValues <- function(QCreportObject) {

  countna <- function(x)
    return(sum(is.na(x))

  # Remove QC lead samples from peakMatrix
  rem_hits <- which(QCreportObject$metaData$samp_lab=="Removed")

  if (length(rem_hits) > 0L) {

    peak_matrix <- QCreportObject$peakMatrix[, -c(rem_hits)]
    samp_lab <- QCreportObject$metaData$samp_lab[-c(rem_hits)]
  } else {
    peak_matrix <- QCreportObject$peakMatrix
    samp_lab <- QCreportObject$metaData$samp_lab
  }

  across_samples <- data.frame(x=apply(peak_matrix, 2L, countna))/nrow(peak_matrix)*100
  across_features <- data.frame(x=apply(peak_matrix, 1L, countna))/ncol(peak_matrix)*100

  QCreportObject$plots$MVplot1 <- ggplot(data=across_samples, aes_(~x)) +
    geom_histogram() +
    xlab("missing values, %") + ggtitle("Missing values per sample") +
    xlim(0L, 100L) + theme_Publication(base_size=12L)

  QCreportObject$plots$MVplot2 <- ggplot(data=across_features, aes_(~x)) +
    geom_histogram() +
    xlab("missing values, %") + ggtitle("Missing values per feature") +
    xlim(0L, 100L) + theme_Publication(base_size=12L)

  cl <- unique(samp_lab)
  out_across_features <- vector("list", length(cl))
  names(out_across_features) <- cl

  # DIMS data have NA replaced with 0
  QCreportObject$peakMatrix[QCreportObject$peakMatrix == 0L] <- NA

  for (slab in seq_len(length(cl))) {
    out_across_features[[slab]] <- apply(cbind(QCreportObject$
      peakMatrix[, samp_lab == cl[slab]], NULL), 1L,
      countna) / length(which(samp_lab == cl[slab])) * 100L
  }

  across_features <- unlist(out_across_features)

  across_features_lab <- NULL
  for (rd in seq_len(length(out_across_features))) {
    across_features_lab <- append(across_features_lab,
      rep(names(out_across_features)[rd], length(out_across_features[[rd]])))
  }

  # Keep QC always the first
  QCreportObject$plots$MVplotClassS <- createClassAndColors(class=samp_lab)
  QCreportObject$plots$MVplotClassF <-
    createClassAndColors(class=across_features_lab)

  across_samples<- data.frame(x=QCreportObject$plots$MVplotClassS$class,
    class=across_samples$x)
  across_features <- data.frame(x=QCreportObject$plots$MVplotClassF$class,
    class=across_features)

  QCreportObject$plots$MVplot3 <- ggplot(data=across_samples,
    aes_(x=~x, y=~class, color=~x)) + geom_boxplot(show.legend=FALSE) +
    ylab("missing values, %") +
    ylim(0L, 100L)+ xlab("") +
    scale_colour_manual(values=QCreportObject$
      plots$MVplotClassS$manual_colors) +
    theme_Publication(base_size=12L) +
    coord_flip()

  QCreportObject$plots$MVplot4 <- ggplot(data=across_features,
    aes_(x=~x, y=~class, color=~x)) + geom_boxplot(show.legend=FALSE) +
    ylab("missing values, %") +
    xlab("") +
    scale_colour_manual(values=QCreportObject$
      plots$MVplotClassF$manual_colors) +
    theme_Publication(base_size=12L)+
    ylim(0L, 100L)+
    coord_flip()

  QCreportObject
}
