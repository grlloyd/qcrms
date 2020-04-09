context("DIMS data")

test_that("createQCreportObject fails with wrong input file", {
    expect_error(createQCreportObject(data_file="MTBLS79.txt", projectdir=".",
        metaData_file="meta_data.txt"))
})

test_that("createQCreportObject can parse csv text file inputs", {
    peak_matrix <- SummarizedExperiment::assay(pmp::MTBLS79)[1:100, ]
    meta_data <- SummarizedExperiment::colData(pmp::MTBLS79)
    meta_data$Sample <- rownames(meta_data)

    # Simulate sample 4 and 5 as blank
    peak_matrix[seq(1, 100, 2), 4:5] <- NA
    peak_matrix[c(1:30), 4:5] <- NA
    peak_matrix[, 4:5] <- peak_matrix[, 4:5] / 25
    peak_matrix[c(46, 78), 4:5] <- peak_matrix[c(46, 78), 4:5] * 10^6
    
    meta_data$Class[4:5] <- "Blank"
    
    temp_dir <- tempdir()
    write.csv(file=file.path(temp_dir,"qcrms_test_file.csv"), peak_matrix)
    write.csv(file=file.path(temp_dir,"qcrms_test_meta_data_file.csv"),
        meta_data, row.names=FALSE)
    expect_message(QCreport <- createQCreportObject(
        data_file="qcrms_test_file.csv", projectdir=temp_dir,
        metaData_file="qcrms_test_meta_data_file.csv"),
        regexp="The number of NA and <= 0 values in peaksData before QC-RSC: 350")

    expect_equal(as.vector(QCreport$peakMatrix[1, 1:3]),
        c(28041.86, 28764.55, 27063.78))
    expect_equal(QCreport$pca_scores_labels, "all")
    
    expect_equal(QCreport$filtering$glog_lambda_filtered, 113874336,
        tolerance=10)
    
    expect_equivalent(QCreport$filtering$table,
        structure(
            list(
                    Filter=structure(c(1L, 2L, 5L, 4L, 3L),
                    .Label=c("Before filtering",
                                "Blank, fold_change=20, fraction=0",
                                "Featrues, method=across, fraction=0.5",
                                "Features, method=QC, fraction=0.9",
                                "MV Sample, max_perc_mv=0.5"),
                    class="factor"),
                    `Number of features`=c(100L, 95L, 95L, 83L, 83L),
                    `Number of samples`=c(172L, 172L, 170L, 170L, 170L),
                    Applied=c(TRUE, TRUE, TRUE, TRUE, TRUE)),
            row.names=c(NA, -5L), class="data.frame")
    )
    
    expect_equal(QCreport$filtering$samples_removed,
        c("batch01_C05", "batch01_S07"))
    
    expect_equal(QCreport$metaData$classColumn, "Class")
    
    expect_equal(as.character(QCreport$metaData$table$Sample[3]),
        "batch01_QC03")
    expect_equal(QCreport$metaData$table$Class[56], "S")
    
    # Plots
    expect_equal(QCreport$plots$ticplot_1$data[3,1], 4345633, tolerance=0.16)
    expect_equal(QCreport$plots$ticplot_2$data[5,1], 3826233650.09348)
    expect_equal(QCreport$plots$ticplot_3$data[6,3], 99)
    expect_equal(QCreport$plots$ticplot_4$data$nPeak[161], 95)
    
    expect_equal(QCreport$plots$PCAallSamples$data$pc1[67], -5.555397, 
        tolerance=1^-7)
    expect_equal(QCreport$plots$PCAQCsamples$data$pc2[5], 4.676568,
        tolerance=1^-7)
    expect_equal(QCreport$plots$PCAQCleading$data$pc1[13], 4.863442,
        tolerance=1^-7)
    expect_equal(QCreport$plots$PCAallQCleading$data$pc2[10], -3.96314994724299)
    
    expect_equal(QCreport$plots$MVplot1$data$x[109], 13)
    expect_equal(QCreport$plots$MVplot2$data$x[99], 18.91892,
        tolerance=1^-7)
    expect_equal(QCreport$plots$MVplot3$data$class[136], 7)
    expect_equal(QCreport$plots$MVplot4$data$class[288], 32.529659395331)
    
    expect_equal(QCreport$plots$RSDplot1$data$list_object[153], 30.63364,
        tolerance=1^-7)
    expect_equal(QCreport$plots$RSDplot2$data$RSD[97], 7.758575,
        tolerance=1^-7)
    
    expect_equal(QCreport$plots$QCplot1$data$values[400], -1.104539,
                 tolerance=1^-7)
    expect_equal(QCreport$plots$QCplot1$data$values[1123], -0.5477665,
                 tolerance=1^-7)
    
    expect_equal(QCreport$plots$SBPCAbefore$data$pc1[34], -2.18008575589513)
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
    expect_equal(QCreport$data$PCAinF$classes[56], "S")
    expect_equal(QCreport$data$PCAinF$RSD$variability_method, "RSD")
    expect_equal(names(QCreport$data$PCAinF$RSD$C[45]), "113.02091")
    expect_equivalent(QCreport$data$PCAinF$RSD$C[45], 38.38334,
        tolerance=1^-7)
    expect_equivalent(QCreport$data$PCAinF$RSD$S[45], 37.46615,
        tolerance=1^-7)
    expect_equivalent(QCreport$data$PCAinF$RSD$QC[45], 21.16112,
        tolerance=1^-7)
    
    expect_equal(QCreport$excludeQC, c(1:5))
    
    expect_equivalent(QCreport$projectHeader[3, 2], "qcrms_test_file.csv")
    expect_equivalent(QCreport$projectHeader[6, 2], "172")
    expect_equivalent(QCreport$projectHeader[7, 2], "QC, Blank, C, S")
    
    expect_equivalent(QCreport$peakPickingParams[1,2], "100")
    
    expect_equivalent(QCreport$TICs[13], 4991575, tolerance=0.5)
    expect_equivalent(QCreport$TICs[76], 4679116, tolerance=0.5)
    
    expect_equivalent(QCreport$TICraw[13], 4991575, tolerance=0.5)
    expect_equivalent(QCreport$TICraw[76], 4679116, tolerance=0.5)
    
    expect_null(QCreport$TICdata[[3]])
    
    expect_equivalent(QCreport$samp.sum[c(1, 33, 67), ],
        data.frame(
            Sample=c("batch01_QC01", "batch02_C07", "batch03_S08"),
            `Measurement time`=c(NA, NA, NA), Class=c("QC", "C", "S"),
            `Number of peaks`=c(99, 95, 98),
            `mzML file size (MB)`=as.numeric(c(NA, NA, NA)),
        check.names=FALSE))
    
    expect_equal(QCreport$QC_hits[11:14], c(47, 53, 59, 65))
    
    context("Test that QC report pdf file is created")
    expect_warning(qcrms::createQCreport(QCreport))
    expect_true(file.exists(file.path(temp_dir, "qcrms_test_file.csv.pdf")))
    
    context ("DIMS: Create metadata file from sample filenames.")
    expect_message(QCreport <- 
        createQCreportObject(data_file="qcrms_test_file.csv", 
        projectdir=temp_dir, excludeQC=NULL)
    )
    expect_equal(QCreport$metaData$table[1:7, ],
        structure(
            list(
                Sample = c("batch01_QC01", "batch01_QC02", "batch01_QC03",
                    "batch01_C05", "batch01_S07", "batch01_C10",
                    "batch01_QC04"), 
                Class = c("QC", "QC", "QC", "sample", "sample", "sample", 
                    "QC"),
                injection_order = 1L:7L, 
                batch = c(1L, 1L, 1L, 1L, 1L, 1L, 1L)
            ),
        row.names = c(NA, 7L),
        class = "data.frame")
    )
    
    context ("DIMS: check output if blank sample is present")
    
    unlink(file.path(temp_dir, "qcrms_test_file.csv"))
    unlink(file.path(temp_dir, "qcrms_test_meta_data_file.csv"))
    unlink(file.path(temp_dir, "qcrms_test_file.csv.xlsx"))
    unlink(file.path(temp_dir, "qcrms_test_file.csv.pdf"))
})
