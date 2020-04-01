context("LCMS data")

test_that("createQCreportObject works with XCMS LCMS data outputs", {
    require(xcms)
    ## Get the full path to the CDF files
    cdfs <- dir(system.file("cdf", package="faahKO"), full.names=TRUE,
        recursive=TRUE)
    ## Create a phenodata data.frame
    meta_data <- data.frame(Sample=sub(basename(cdfs), pattern=".CDF",
        replacement="", fixed=TRUE),
        sample_group=c(rep("QC", 6), "MSMS",rep("WT", 5)),
        remove=c(TRUE, rep(NA, 11)),
        stringsAsFactors=FALSE)
    rownames(meta_data) <- meta_data$Sample
    
    expect_message(raw_data <- readMSData(files=cdfs,
        pdata=new("NAnnotatedDataFrame", meta_data), mode="onDisk"))
    cwp <- xcms::CentWaveParam(peakwidth = c(20, 80), noise=5000)
    xdata <- xcms::findChromPeaks(raw_data, param=cwp)
    xdata <- adjustRtime(xdata, param=ObiwarpParam(binSize=0.6))
    xdata <- xcms::groupChromPeaks(xdata,
        xcms::PeakDensityParam(sampleGroups=rep("S", 12)))
    
    temp_dir <- tempdir()
    write.csv(file=file.path(temp_dir, "qcrms_test_meta_data_file.csv"),
        meta_data, row.names=F)
    save(file=file.path(temp_dir, "LCMS_xdata.Rdata"), list="xdata")
    
    expect_warning(QCreport <- createQCreportObject(
        data_file="LCMS_xdata.Rdata", projectdir=temp_dir,
        metaData_file="qcrms_test_meta_data_file.csv", xcms_output="xdata",
        classLabel="sample_group", excludeQC="remove", msms_label="MSMS"))

    expect_true(file.exists(file.path(temp_dir, "LCMS_xdata.xlsx")))
    
    expect_equal(QCreport$xcms_output, "xdata")
    expect_equal(QCreport$pca_scores_labels, "all")
    expect_equal(QCreport$data_file, "LCMS_xdata.Rdata")
    expect_equal(QCreport$projectdir, temp_dir)
    
    expect_equal(QCreport$filtering$glog_lambda_filtered, 61043890465)
    expect_equivalent(QCreport$filtering$table[c(1, 4), ],
        data.frame(
            Filter=c("Before filtering", "Features, method=QC, fraction=0.9"),
            `Number of features`=c(268, 128),
            `Number of samples`=c(11, 11),
            Applied=c(TRUE, TRUE),
        check.names=FALSE)
    )
    
    #expect_equivalent(QCreport$xset@phenoData[c(1, 4, 9), ],
    #    data.frame(
    #        Sample=c("ko15", "ko19", "wt18")
    #    check.names=FALSE)
    #)
    
    #expect_equal(as.vector(QCreport$peakMatrix[1, 1:3]),
    #    c(28041.86, 28764.55, 27063.78))
    #expect_equal(QCreport$pca_scores_labels, "all")
    

    
    #expect_equal(QCreport$metaData$classColumn, "Class")
    
    #expect_equal(as.character(QCreport$metaData$table$Sample[3]),
    #    "batch01_QC03")
    #expect_equal(QCreport$metaData$table$Class[56], "S")
    
    # Plots
    #expect_equal(QCreport$plots$ticplot_1$data[3,1], 4345633, tolerance=0.16)
    #expect_equal(QCreport$plots$ticplot_2$data[5,1], 4412936)
    #expect_equal(QCreport$plots$ticplot_3$data[6,3], 95)
    #expect_equal(QCreport$plots$ticplot_4$data$nPeak[161], 89)
    
    #expect_equal(QCreport$plots$PCAallSamples$data$pc1[67], -5.555397, 
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$PCAQCsamples$data$pc2[5], 4.676568,
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$PCAQCleading$data$pc1[13], 4.863442,
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$PCAallQCleading$data$pc2[10], 4.136927,
    #    tolerance=1^-7)
    
    #expect_equal(QCreport$plots$MVplot1$data$x[109], 13)
    #expect_equal(QCreport$plots$MVplot2$data$x[99], 18.91892,
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$MVplot3$data$class[136], 7)
    #expect_equal(QCreport$plots$MVplot4$data$class[288], 102.040816,
    #    tolerance=1^-7)
    
    #expect_equal(QCreport$plots$RSDplot1$data$list_object[153], 30.63364,
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$RSDplot2$data$RSD[97], 7.758575,
    #    tolerance=1^-7)
    
    #expect_equal(QCreport$plots$QCplot1$data$values[400], -1.104539,
    #             tolerance=1^-7)
    #expect_equal(QCreport$plots$QCplot1$data$values[1123], -0.5477665,
    #             tolerance=1^-7)
    
    #expect_equal(QCreport$plots$SBPCAbefore$data$pc1[34], 1.77536,
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$SBPCAbeforeQC$data$pc1[3], -8.776818,
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$SBRSDbefore$data$list_object[43], 38.38334,
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$SBPCAfter$data$pc1[56], -2.820385,
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$SBPCAfterQC$data$pc1[4], -3.94937,
    #    tolerance=1^-7)
    #expect_equal(QCreport$plots$SBRSDafter$data$list_object[78], 62.04692, 
    #    tolerance=1^-7)
    
    # Tables
    #expect_equal(QCreport$tables$corrMatrix[1, 1], 0.56, tolerance=1^-7)
    #expect_equal(QCreport$tables$corrMatrix[2, 2], 1.041714e-15,
    #    tolerance=1^-7)
    #expect_equal(QCreport$tables$RSDtable1[2, 4], 40.2225, tolerance=1^-7)
    #expect_equal(QCreport$tables$SBtableBefore[1, 3], 33.62639, tolerance=1^-7)
    #expect_equal(QCreport$tables$SBtableAfter[1, 3], 56.40074, tolerance=1^-7)
    
    # data
    #expect_equal(QCreport$data$PCAinF$Data[4, 56], 15362.38, tolerance=1^-7)
    #expect_equal(QCreport$data$PCAinF$classes[56], "C")
    #expect_equal(QCreport$data$PCAinF$RSD$variability_method, "RSD")
    #expect_equal(names(QCreport$data$PCAinF$RSD$C[45]), "113.02091")
    #expect_equivalent(QCreport$data$PCAinF$RSD$C[45], 38.38334,
    #    tolerance=1^-7)
    #expect_equivalent(QCreport$data$PCAinF$RSD$S[45], 37.46615,
    #    tolerance=1^-7)
    #expect_equivalent(QCreport$data$PCAinF$RSD$QC[45], 21.16112,
    #    tolerance=1^-7)
    
    #expect_equal(QCreport$excludeQC, c(1:5))
    
    #expect_equivalent(QCreport$projectHeader[3, 2], "qcrms_test_file.csv")
    #expect_equivalent(QCreport$projectHeader[6, 2], "172")
    #expect_equivalent(QCreport$projectHeader[7, 2], "QC, C, S")
    
    #expect_equivalent(QCreport$peakPickingParams[1,2], "100")
    
    #expect_equivalent(QCreport$TICs[13], 4991575, tolerance=0.5)
    #expect_equivalent(QCreport$TICs[76], 4679116, tolerance=0.5)
    
    #expect_equivalent(QCreport$TICraw[13], 4991575, tolerance=0.5)
    #expect_equivalent(QCreport$TICraw[76], 4679116, tolerance=0.5)
    
    #expect_null(QCreport$TICdata[[3]])
    
    #expect_equivalent(QCreport$samp.sum[c(1, 33, 67), ],
    #    data.frame(
    #        Sample=c("batch01_QC01", "batch02_C07", "batch03_S08"),
    #        `Measurement time`=c(NA, NA, NA), Class=c("QC", "C", "S"),
    #        `Number of peaks`=c(99, 95, 98),
    #        `mzML file size (MB)`=as.numeric(c(NA, NA, NA)),
    #    check.names=FALSE))
    
    #expect_equal(QCreport$QC_hits[11:14], c(47, 53, 59, 65))
    
    context("Test that QC report pdf file is created")
    expect_warning(qcrms::createQCreport(QCreport))
    expect_true(file.exists(file.path(temp_dir, "LCMS_xdata.pdf")))
    expect_true(file.exists(file.path(temp_dir, "LCMS_xdata_EICs.pdf")))
    
    unlink(file.path(temp_dir, "LCMS_xdata.Rdata"))
    unlink(file.path(temp_dir, "qcrms_test_meta_data_file.csv"))
    unlink(file.path(temp_dir, "LCMS_xdata.xlsx"))
    unlink(file.path(temp_dir, "LCMS_xdata.pdf"))
    unlink(file.path(temp_dir, "LCMS_xdata_EICs.pdf"))
})
