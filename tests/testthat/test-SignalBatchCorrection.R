context ("Test signal/batch correction function")

QCreportObject <- qcrms:::testData$QCreportObject

MTBLS79 <- pmp::MTBLS79[c(10, 530, 780),
    pmp::MTBLS79$Batch == 1 | pmp::MTBLS79$Batch == 5]

QCreportObject$peakMatrix <- SummarizedExperiment::assay(MTBLS79)
meta_data <- SummarizedExperiment::colData(MTBLS79)
meta_data$Sample <- rownames(meta_data)
meta_data$batch <- meta_data$Batch
meta_data$injection_order <- seq_len(nrow(meta_data))

QCreportObject$metaData$table <- meta_data
QCreportObject$QC_hits <- which(meta_data$Class == "QC")
QCreportObject$metaData$samp_lab <- meta_data$Class

temp_dir <- tempdir()
QCreportObject$xlsxout <- file.path(temp_dir, "qcrms.xlsx")

QCreportObject <- qcrms:::createXlsx(QCreportObject)
expect_warning(QCreportObject <- qcrms:::SignalBatchCorrection(QCreportObject))

expect_equal(QCreportObject$filtering$table$Applied, c(TRUE, FALSE,TRUE,
    TRUE, TRUE))

context("Multiple plots per batch should exist")
expect_equal(length(QCreportObject$plots$plots_per_batch_pca), 2)

unlink (file.path(temp_dir, "qcrms.xlsx"))
