#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 aes
#'
NULL

#' Plot violin plots from the dolist_object fucntion output
#'
#' @param list_object OUtput of do_variability_list function
#' @param plotTitle Main tilte for the plot
#' @param base_size Font size for plot fonts
#' @param subtitle Subtitle to include in PCA plot
#' @param ylim If not NULL this value will be used for maximum of violin plots
#' @return Ggplot object with plot(s)
#' @export

do_variability_plot <- function(list_object, plotTitle=NULL, base_size=12L,
  subtitle=NULL, ylim=NULL) {
  variability_method <- list_object$variability_method
  if (variability_method=="RSD") variability_method <- "RSD %"

  # Remove flag of which method was used before processing data
  list_object$variability_method <- NULL

  list_objects <- unlist(list_object)

  list_objectsLab <- NULL
  for (rd in seq_len(length(list_object))) {
    list_objectsLab <- append(list_objectsLab, rep(names(list_object)[rd],
      length(list_object[[rd]])))
  }

  plotClass <- createClassAndColors(class=list_objectsLab)

  B <- data.frame(list_object=list_objects, class=plotClass$class)

  if (is.null(ylim)) {
    ylim <- c(min(list_objects, na.rm=TRUE), max(list_objects, na.rm=TRUE))
  }
  if (length(ylim) == 1L) ylim <- c(0L, ylim)

  out <- ggplot(B, aes(y=list_object, x=class, colour=class, fill=class)) +
    geom_violin(alpha=0.1, na.rm=TRUE, show.legend=FALSE,
      draw_quantiles=c(0.25, 0.5, 0.75)) +
    scale_colour_manual(values=plotClass$manual_colors) +
    theme_Publication(base_size=base_size) + xlab("") +
    scale_y_continuous(limits=c(ylim[1L], ylim[2L])) +
    ggtitle(plotTitle, subtitle=subtitle) + ylab(variability_method)
   out
}
