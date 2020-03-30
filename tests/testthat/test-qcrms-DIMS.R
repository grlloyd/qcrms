context("DIMS data")

test_that("createQCreportObject fails with wrong input file", {
    expect_error(createQCreportObject(data_file="MTBLS79.txt", projectdir=".",
        metaData_file="meta_data.txt"))
})

test_that("createQCreportObject can parse csv text file inputs", {
    peak_matrix <- SummarizedExperiment::assay(pmp::MTBLS79)[1:100, ]
    meta_data <- SummarizedExperiment::colData(pmp::MTBLS79)
    meta_data$Sample <- rownames(meta_data)

    temp_dir <- tempdir()
    write.csv(file=file.path(temp_dir,"qcrms_test_file.csv"), peak_matrix)
    write.csv(file=file.path(temp_dir,"qcrms_test_meta_data_file.csv"),
        meta_data)
    expect_warning(QCreport <- createQCreportObject(
        data_file="qcrms_test_file.csv", projectdir=temp_dir,
        metaData_file="qcrms_test_meta_data_file.csv"))

    expect_equal(as.vector(QCreport$peakMatrix[1, 1:3]),
        c(28041.86, 28764.55, 27063.78))
    expect_equal(QCreport$pca_scores_labels, "all")
    expect_equal(QCreport$filtering$glog_lambda_filtered, 80142621)
    expect_equal(QCreport$metaData$classColumn, "Class")
    expect_equal(as.character(QCreport$metaData$table$Sample[3]),
        "batch01_QC03")
    expect_equal(QCreport$metaData$table$Class[56], "S")
    
    # Plots
    expect_equal(QCreport$plots$ticplot_1$data[3,1], 4345633, tolerance=0.16)
    expect_equal(QCreport$plots$ticplot_2$data[5,1], 4412936)
    expect_equal(QCreport$plots$ticplot_3$data[6,3], 95)
    expect_equal(QCreport$plots$ticplot_4$data$nPeak[161], 89)
    
    expect_equal(QCreport$plots$PCAallSamples$data$pc1[67], -5.555397, 
        tolerance=1^-7)
    expect_equal(QCreport$plots$PCAQCsamples$data$pc2[5], 4.676568,
        tolerance=1^-7)
    expect_equal(QCreport$plots$PCAQCleading$data$pc1[13], 4.863442,
        tolerance=1^-7)
    expect_equal(QCreport$plots$PCAallQCleading$data$pc2[10], 4.136927,
        tolerance=1^-7)
    
    expect_equal(QCreport$plots$MVplot1$data$x[109], 13)
    expect_equal(QCreport$plots$MVplot2$data$x[99], 18.91892,
        tolerance=1^-7)
    expect_equal(QCreport$plots$MVplot3$data$class[136], 7)
    expect_equal(QCreport$plots$MVplot4$data$class[288], 102.040816,
        tolerance=1^-7)
    
    expect_equal(QCreport$plots$RSDplot1$data$list_object[153], 30.63364,
        tolerance=1^-7)
    expect_equal(QCreport$plots$RSDplot2$data$RSD[97], 7.758575,
        tolerance=1^-7)
    
    expect_equal(QCreport$plots$QCplot1$data$values[400], -1.104539,
                 tolerance=1^-7)
    expect_equal(QCreport$plots$QCplot1$data$values[1123], -0.5477665,
                 tolerance=1^-7)
    
    expect_equal(QCreport$plots$SBPCAbefore$data$pc1[34], 1.77536,
        tolerance=1^-7)
    expect_equal(QCreport$plots$SBPCAbeforeQC$data$pc1[3], -8.776818,
        tolerance=1^-7)
    expect_equal(QCreport$plots$SBRSDbefore$data$list_object[43], 38.38334,
        tolerance=1^-7)
    expect_equal(QCreport$plots$SBPCAfter$data$pc1[56], -2.820385,
        tolerance=1^-7)
    expect_equal(QCreport$plots$SBPCAfterQC$data$pc1[4], -3.94937,
        tolerance=1^-7)
    expect_equal(QCreport$plots$SBRSDafter$data$list_object[78], 62.04692, 
        tolerance=1^-7)
    
    # Tables
    expect_equal(QCreport$tables$corrMatrix[1, 1], 0.56, tolerance=1^-7)
    expect_equal(QCreport$tables$corrMatrix[2, 2], 1.041714e-15,
        tolerance=1^-7)
    expect_equal(QCreport$tables$RSDtable1[2, 4], 40.2225, tolerance=1^-7)
    expect_equal(QCreport$tables$SBtableBefore[1, 3], 33.62639, tolerance=1^-7)
    expect_equal(QCreport$tables$SBtableAfter[1, 3], 56.40074, tolerance=1^-7)
    
    # data
    expect_equal(QCreport$data$PCAinF$Data[4, 56], 15362.38, tolerance=1^-7)
    expect_equal(QCreport$data$PCAinF$classes[56], "C")
    expect_equal(QCreport$data$PCAinF$RSD$variability_method, "RSD")
    expect_equal(names(QCreport$data$PCAinF$RSD$C[45]), "113.02091")
    expect_equivalent(QCreport$data$PCAinF$RSD$C[45], 38.38334,
        tolerance=1^-7)
    expect_equivalent(QCreport$data$PCAinF$RSD$S[45], 37.46615,
        tolerance=1^-7)
    expect_equivalent(QCreport$data$PCAinF$RSD$QC[45], 21.16112,
        tolerance=1^-7)
    
    expect_equal(QCreport$excludeQC, c(1:5))
    
    context("Test that QC report pdf file is created")
    expect_warning(qcrms::createQCreport(QCreport))
    expect_true(file.exists(file.path(temp_dir, "qcrms_test_file.csv.pdf")))

    unlink(file.path(temp_dir, "qcrms_test_file.csv"))
    unlink(file.path(temp_dir, "qcrms_test_meta_data_file.csv"))
    unlink(file.path(temp_dir, "qcrms_test_file.csv.xlsx"))
    unlink(file.path(temp_dir, "qcrms_test_file.csv.pdf"))
})
