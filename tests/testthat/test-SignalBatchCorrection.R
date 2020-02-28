context ("Test signal/batch correction function")

QCreportObject <- testData$QCreportObject
QCreportObject$peakMatrix <- 
    SummarizedExperiment::assay(pmp::MTBLS79)[1L:100L, ]
meta_data <- SummarizedExperiment::colData(pmp::MTBLS79)
meta_data$Sample <- rownames(meta_data)
meta_data$batch <- meta_data$Batch
QCreportObject$metaData$table <- meta_data
QCreportObject$QC_hits <- which(meta_data$Class == "QC")

temp_dir <- tempdir()
QCreportObject$xlsxout <- file.path(temp_dir, "qcrms.xlsx")

qcrms:::createXlsx(QCreportObject)
#QCreportObject <- qcrms:::SignalBatchCorrection(QCreportObject)

unlink (file.path(temp_dir, "qcrms.xlsx"))
