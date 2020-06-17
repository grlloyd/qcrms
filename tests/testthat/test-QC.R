context ("Test QC function for multiple batches")

QCreportObject <- testData$QCreportObject
QCreportObject$peakMatrix <- 
    SummarizedExperiment::assay(pmp::MTBLS79)[1L:100L, ]
meta_data <- SummarizedExperiment::colData(pmp::MTBLS79)
meta_data$Sample <- rownames(meta_data)
meta_data$batch <- meta_data$Batch
QCreportObject$metaData$table <- meta_data
QCreportObject$QC_hits <- which(meta_data$Class == "QC")

QCreportObject <- qcrms:::QC(QCreportObject)
expect_true(!is.null(QCreportObject$plots$QCplot1))
expect_true(!is.null(QCreportObject$plots$QCplot2))
