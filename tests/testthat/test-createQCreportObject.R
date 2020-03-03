context("createQCreportObject fails with wrong input file")

expect_error(createQCreportObject(data_file="MTBLS79.txt", projectdir=".",
    metaData_file="meta_data.txt"))

context("createQCreportObject can parse simple text file inputs")

peak_matrix <- SummarizedExperiment::assay(pmp::MTBLS79)[1:100, ]
meta_data <- SummarizedExperiment::colData(pmp::MTBLS79)
meta_data$Sample <- rownames(meta_data)

temp_dir <- tempdir()
write.csv(file=file.path(temp_dir,"qcrms_test_file.csv"), peak_matrix)
write.csv(file=file.path(temp_dir,"qcrms_test_meta_data_file.csv"),
    meta_data)
expect_warning(QCreport <- createQCreportObject(data_file="qcrms_test_file.csv",
    projectdir=temp_dir,
    metaData_file="qcrms_test_meta_data_file.csv"))

expect_equal(as.vector(QCreport$peakMatrix[1, 1:3]),
    c(28041.86, 28764.55, 27063.78))

unlink(file.path(temp_dir, "qcrms_test_file.csv"))
unlink(file.path(temp_dir, "qcrms_test_meta_data_file.csv"))
unlink(file.path(temp_dir, "qcrms_test_file.csv.xlsx"))
