
#' Create summary table of list object containing variabilty measures,
#' for example output object of do_variability list function
#'
#' @param list_object OUtput of dolist_object function
#' @param QC_label Label used for QC samples. If set to NULL, assumes that no
#' QC samples are present in data set
#' @param Blank_label Label used for Blank samples
#' @return Table of list_object% values per group
#' @export

do_variability_table <- function(list_object, QC_label="QC",
  Blank_label="Blank") {
  list_object$variability_method <- NULL

  tableCNames <- names(list_object)
  tableCNames <- createClassAndColors(class=tableCNames, QC_label=QC_label,
    Blank_label=Blank_label)$class

  tableData <- lapply(list_object, summary)

  # Summary function doesn't return NA count if there are no NA's
  for (i in seq_len(length(tableData))) {
    if (length(tableData[[i]]) < 7L) {
      tableData[[i]] <- append(tableData[[i]], 0L)
    }
  }

  tableData <- do.call(rbind, tableData)
  colnames(tableData)[7L] <- "NA's"

  tableData <- as.data.frame(tableData)

  rownames(tableData) <- names(list_object)

  tableData <- tableData[order(factor(rownames(tableData), levels=tableCNames,
    ordered=TRUE)), ]
  tableData
}
