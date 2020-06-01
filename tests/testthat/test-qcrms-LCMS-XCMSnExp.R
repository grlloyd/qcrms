context("LCMS data, XCMSnExp")

library(xcms)
library(BiocManager)
if(!requireNamespace("msPurityData", quietly=TRUE)){
    BiocManager::install("msPurityData")
}

if(!requireNamespace("faahKO", quietly=TRUE)){
    BiocManager::install("faahKO")
}

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
    xdata <- adjustRtime(xdata, param=ObiwarpParam(binSize=0.6, center=6))
    xdata <- xcms::groupChromPeaks(xdata,
        xcms::PeakDensityParam(sampleGroups=rep("S", 12)))

    temp_dir <- tempdir()
    write.csv(file=file.path(temp_dir, "qcrms_test_meta_data_file.csv"),
        meta_data, row.names=F)
    save(file=file.path(temp_dir, "LCMS_xdata.Rdata"), list="xdata")

    # Create meta data xlsx file format
    require (openxlsx)
    wb <- createWorkbook()
    addWorksheet (wb,"meta")
    writeData (wb,"meta", meta_data, rowNames = F)
    saveWorkbook (wb, file.path(temp_dir, "qcrms_meta_data.xlsx"),
        overwrite = T)
    
    expect_warning(QCreport <- createQCreportObject(
        data_file="LCMS_xdata.Rdata", projectdir=temp_dir,
        metaData_file="qcrms_test_meta_data_file.csv", xcms_output="xdata",
        classLabel="sample_group", excludeQC="remove", msms_label="MSMS"))

    expect_true(file.exists(file.path(temp_dir, "LCMS_xdata.xlsx")))

    # Check content of output xlsx file
    wb_temp <- openxlsx::loadWorkbook(file.path(temp_dir, "LCMS_xdata.xlsx"))
    
    variableMetaData <- readWorkbook(wb_temp, "variableMetaData")
    expect_equal(head (variableMetaData),
        structure(list(name = c("M205T2791", "M206T2790", "M207T2719", 
            "M233T3029", "M241T3686", "M244T2835"),
        mz=c(205, 206, 207.100006103516, 233, 241.100006103516,
            244.100006103516),
        mzmin=c(205, 206, 207.100006103516, 233, 241.100006103516,
            244.100006103516),
        mzmax=c(205, 206, 207.100006103516, 233, 241.199996948242,
            244.100006103516),
        rt=c(2791.04516601562, 2790.11202148437, 2718.83251953125, 
            3029.41455078125, 3686.39326269531, 2834.84826660156),
        rtmin=c(2787.63159179688, 2786.41528320312, 2712.60815429688,
            3014.683, 3671.52124023438,  2830.10083007812),
        rtmax=c(2795.03662109375, 2795.61206054688, 2723.601,
            3063.19799804688, 3740.9423828125, 2836.123046875), 
        npeaks=c(12, 12, 13, 11, 20, 6),
        S=c(12, 11, 9, 9, 11, 6), 
        ms_level = c(1, 1, 1, 1, 1, 1)),
        row.names=c(NA, 6L), class="data.frame")
    )
    rm (wb_temp)
    
    expect_equal(QCreport$xcms_output, "xdata")
    expect_equal(QCreport$pca_scores_labels, "all")
    expect_equal(QCreport$data_file, "LCMS_xdata.Rdata")
    expect_equal(QCreport$projectdir, temp_dir)

    expect_equal(QCreport$filtering$glog_lambda_filtered, 61043890464.6641)
    expect_equal(QCreport$filtering$glog_lambda_filtered_SB, 1338305112712.85)
    expect_equivalent(QCreport$filtering$table[c(1, 4), ],
        data.frame(
            Filter=c("Before filtering", "Features, method=QC, fraction=0.9"),
            `Number of features`=c(268, 128),
            `Number of samples`=c(11, 11),
            Applied=c(TRUE, TRUE),
        check.names=FALSE, stringsAsFactors=FALSE)
    )

    expect_equivalent(pData(QCreport$xset@phenoData)[c(1, 4, 9), ],
        structure(list(
            Sample = c("ko15", "ko19", "wt19"),
            sample_group = c("QC", "QC", "WT"),
            remove = c(TRUE, NA, NA)),
            row.names = c("ko15", "ko19", "wt19"),
            class = "data.frame")
    )

    expect_equivalent(xcms::chromPeaks(QCreport$xset)[1, ],
       c(453.2000122, 453.2000122, 453.2000122, 2509.2795410, 2501.3779297,
            2528.2773438, 1007408.9731765, 1007380.8042353, 38152.0000000,
            38151.0000000, 1)
    )

    expect_equal(xcms::featureDefinitions(QCreport$xset)$peakidx[[47]], 
        c(1405, 2436, 2854, 3896, 4299, 4982, 4990))

    expect_equivalent(xcms::rtime(QCreport$xset, adjusted=FALSE,
        bySample=TRUE)[[5]][10:15],
        c(2515.462, 2517.027, 2518.592, 2520.157, 2521.722, 2523.287))
    expect_equivalent(xcms::rtime(QCreport$xset, adjusted=TRUE,
        bySample=TRUE)[[5]][10:15],
        c(2514.697510, 2516.180908, 2517.664795, 2519.149170, 2520.634277,
            2522.120361))

    expect_equal(QCreport$xset@.processHistory[[1]]@type, "Peak detection")
    expect_equal(QCreport$xset@.processHistory[[1]]@param@ppm, 25)
    expect_equal(QCreport$xset@.processHistory[[1]]@param@peakwidth, c(20, 80))
    expect_equal(QCreport$xset@.processHistory[[1]]@param@snthresh, 10)
    expect_equal(QCreport$xset@.processHistory[[1]]@param@mzdiff, -0.001)

    expect_equal(QCreport$peakMatrix[1, 1:3],
        c(ko15.CDF=1924712.01585714, ko16.CDF=1757150.9648,
            ko18.CDF=1714581.77647058))

    expect_equal(QCreport$metaData$table,
        structure(list(Sample = c("ko15", "ko16", "ko18", "ko19", "ko21",
            "ko22", "wt16", "wt18", "wt19", "wt21", "wt22"),
        xcms_class = c("QC", "QC", "QC", "QC", "QC", "QC", "WT", "WT", "WT",
            "WT", "WT"), 
        sample_group = c("Removed", "QC", "QC", "QC", "QC", "QC", "WT", "WT",
            "WT", "WT", "WT"),
        remove = c(TRUE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        injection_order = 1:11, batch = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
            1L, 1L)),
        row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L),
        class = "data.frame")
    )

    expect_equal(QCreport$pca_scores_labels, "all")
    expect_equal(QCreport$metaData$classColumn, "sample_group")

    # Plots
    expect_equal(QCreport$plots$ticplot_1$data[3,1], 754424703.6)
    expect_equal(QCreport$plots$ticplot_2$data[5,1], 536643304)
    expect_equal(QCreport$plots$ticplot_3$data[6,3], 325)
    expect_equal(QCreport$plots$ticplot_4$data$nPeak[7], 507)
    expect_equal(QCreport$plots$ticplot_5$data$file.size[7], 3.56)

    expect_equal(QCreport$plots$PCAallSamples$data$pc1[4], 6.47846949411406)
    expect_equal(QCreport$plots$PCAQCsamples$data$pc2[5], 0.335577926713068)
    expect_equal(QCreport$plots$PCAQCleading$data$pc1[4], 7.56288127255809)
    expect_equal(QCreport$plots$PCAallQCleading$data$pc2[7], -7.24165986257958)

    expect_equal(QCreport$plots$MAD_rt$data$list_object[34], 4.15956171093739)
    expect_equal(QCreport$plots$peak_width$data$list_object[34],
        46.808349609375)
    expect_equal(QCreport$plots$mz_median$data$list_object[523],
        -69.3985738677017)

    expect_equal(QCreport$plots$EICs[[1]]$data$rt[34], 3554.39404296875)
    expect_equal(QCreport$plots$EICs[[1]]$data$mz[45], 508.200012207031)
    expect_equal(QCreport$plots$EICs[[1]]$data$i[45], 362624)
    expect_equal(QCreport$plots$EICs[[1]]$data$Class[45],
        structure(2L, .Label=c("QC", "Removed", "WT"), class = c("ordered", 
        "factor")))
    expect_equal(QCreport$plots$EICs[[1]]$data$sample[45], 1L)

    expect_equal(QCreport$plots$MVplot1$data$x[7], 17.910447761194)
    expect_equal(QCreport$plots$MVplot2$data$x[10], 50)
    expect_equal(QCreport$plots$MVplot3$data$class[6], 20.1492537313433)
    expect_equal(QCreport$plots$MVplot4$data$class[531], 400)

    expect_equal(QCreport$plots$RSDplot1$data$list_object[153],
        42.1033162472477)
    expect_equal(QCreport$plots$RSDplot2$data$RSD[97], 11.901710037869)

    expect_equal(QCreport$plots$QCplot1$data$values[3], 0.126531150918856)
    expect_equal(QCreport$plots$QCplot1$data$values[1123],
        -0.00868209214666562)
    expect_equal(QCreport$plots$QCplot2$data$value[3], 0.64753837592395)
    expect_equal(QCreport$plots$QCplot2$data$value[1123], -0.380990238832714)

    expect_equal(QCreport$plots$SBPCAbefore$data$pc1[4], 3.55062795302708)
    expect_equal(QCreport$plots$SBPCAbeforeQC$data$pc1[3], 5.44564141304822)
    expect_equal(QCreport$plots$SBRSDbefore$data$list_object[43],
        26.0975634461339)
    expect_equal(QCreport$plots$SBPCAfter$data$pc1[6], -0.370741378151436)
    expect_equal(QCreport$plots$SBPCAfterQC$data$pc1[4], 5.89277901023488)
    expect_equal(QCreport$plots$SBRSDafter$data$list_object[78],
        2.30445829090211)

    # Tables
    expect_equal(QCreport$tables$corrMatrix[2, 1], -0.57)
    expect_equal(QCreport$tables$corrMatrix[2, 2], 0.0650941627344147)

    expect_equal(QCreport$tables$RT_rsd,
        structure(list(
            Min.=c(0.00318857359890208, 0.00511148124967863),
            `1st Qu.`=c(0.0840254897400688, 0.115441585461868),
            Median=c(0.136879091709147, 0.238908162172896),
            Mean=c(0.285418323971856, 0.34243500053187),
            `3rd Qu.`=c(0.367293950976682, 0.511650919161229),
            Max.=c(2.29359102554849, 2.08891122720008),
            `NA's`=c(8, 10)),
            row.names=c("QC", "WT"),
        class = "data.frame")
    )

    expect_equal(QCreport$tables$RT_mad, 
        structure(list(
            Min.=c(0, 0),
            `1st Qu.`=c(1.7177122716795, 2.31271664428711),
            Median=c(3.7412484375, 5.00069831542969),
            Mean=c(8.60809614984987, 9.08930910256126),
            `3rd Qu.`=c(10.419011315918, 11.7209465881348),
            Max.=c(112.605569384766, 55.1881199707031),
            `NA's`=c(0, 4)),
            row.names = c("QC", "WT"),
        class = "data.frame")
    )

    expect_equal(QCreport$tables$peak_width, 
        structure(list(
            Min.=c(12.33056640625, 7.738037109375),
            `1st Qu.`=c(38.6731630859375, 39.0038757324219),
            Median=c(47.4981689453125, 48.1656494140625),
            Mean=c(48.9647423296117, 49.7862030954072),
            `3rd Qu.`=c(56.3682250976562, 56.9166870117188),
            Max.=c(146.4296875, 108.039428710938),
            `NA's`=c(0, 4)), 
            row.names=c("QC", "WT"),
        class="data.frame")
    )

    expect_equal(QCreport$tables$mz_median, 
        structure(list(
            Min.=c(-272.227294755146, -284.60683892769),
            `1st Qu.`=c(-21.3940776658473, 0),
            Median=c(0, 0),
            Mean=c(-12.9934795359947, 14.5174218106226),
            `3rd Qu.`=c(0, 29.6237475668885),
            Max.=c(120.794903450402, 223.103402367672),
            `NA's`=c(0, 4)),
            row.names=c("QC", "WT"),
        class="data.frame")
    )

    expect_equal(QCreport$tables$RSDtable1[2, 4], 45.8166907329745)
    expect_equal(QCreport$tables$SBtableBefore[1, 3], 32.9090168180649)
    expect_equal(QCreport$tables$SBtableAfter[1, 3], 11.2174961555864)

    # data
    expect_equal(QCreport$data$PCAinF$Data[4, 5], 149097.550000002)
    expect_equal(QCreport$data$PCAinF$classes[6], "WT")
    expect_equal(QCreport$data$PCAinF$RSD$variability_method, "RSD")
    expect_equal(names(QCreport$data$PCAinF$RSD$WT[34]), "FT034")
    expect_equal(QCreport$data$PCAinF$RSD$WT[45], 
        c(`FT045`=61.1306994096235))
    expect_equal(QCreport$data$PCAinF$RSD$QC[45],
        c(`FT045` = 54.3657276849361))

    expect_equal(QCreport$excludeQC, "remove")

    expect_equivalent(QCreport$projectHeader[3, 2], "LCMS_xdata")
    expect_equivalent(QCreport$projectHeader[6, 2], "11")
    expect_equivalent(QCreport$projectHeader[7, 2], "QC, WT")

    expect_equivalent(QCreport$peakPickingParams[1,2], "268")

    expect_equivalent(QCreport$TICs[10], c(`11` = 575133855.059309))

    expect_equivalent(QCreport$TICraw[1], 0)

    expect_null(QCreport$TICdata[[3]])

    expect_equivalent(QCreport$samp.sum[c(1, 5, 7), ],
        data.frame(
            Sample=c("ko15", "ko21", "wt16"),
            `Measurement time`=as.character(c(NA, NA, NA)),
            Class=c("QC", "QC", "WT"),
            `Number of peaks`=c(561, 276, 507),
            `mzML file size (MB)`=c(3.52, 3.58, 3.56),
        check.names=FALSE)
    )

    expect_equal(QCreport$QC_hits, 1L:6L)

    context("Test that QC report pdf file is created")
    expect_warning(qcrms::createQCreport(QCreport))
    expect_true(file.exists(file.path(temp_dir, "LCMS_xdata.pdf")))
    expect_true(file.exists(file.path(temp_dir, "LCMS_xdata_EICs.pdf")))

    expect_equal(QCreport$peakPickingParams,
        structure(
            c("Number of peak groups:", "method", "ppm", "peakwidth", "mzdif",
                "snthresh", "integrate", "noise", "prefilter", "268",
                "CentWave", "25", "20-80", "-0.001", "10", "1", "5000",
                "3, 100"),
            .Dim=c(9L, 2L), .Dimnames=list(NULL, c("", ""))
        )
    )

    context("Using xlsx input for meta data returns the same output.")
    expect_warning(QCreport <- createQCreportObject(
        data_file="LCMS_xdata.Rdata", projectdir=temp_dir,
        metaData_file="qcrms_meta_data.xlsx", xcms_output="xdata",
        classLabel="sample_group", excludeQC="remove", msms_label="MSMS",
        plot_eic=FALSE)
    )
    
    expect_equal(QCreport$metaData$file, "qcrms_meta_data.xlsx")

    expect_equal(QCreport$metaData$table,
        structure(list(Sample = c("ko15", "ko16", "ko18", "ko19", "ko21",
            "ko22", "wt16", "wt18", "wt19", "wt21", "wt22"),
        xcms_class = c("QC", "QC", "QC", "QC", "QC", "QC", "WT", "WT", "WT",
            "WT", "WT"),
        sample_group = c("Removed", "QC", "QC", "QC", "QC", "QC", "WT", "WT",
            "WT", "WT", "WT"),
        remove = c(TRUE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        injection_order = 1:11, batch = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
            1L, 1L)),
        row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L),
        class = "data.frame")
    )
    
    context("Use classQC column to fill in missing sample labels. And reading
        from metaData sheet in xlsx file. Locating raw data files in
        different folder than xcms object.")
    
    raw_paths <- QCreport$raw_paths
    # On M$ Windows xcms keeps "\\"
    raw_paths <- gsub("\\\\", "/", QCreport$raw_paths)
    
    meta_data$ClassQC <- meta_data$sample_group
    meta_data$sample_group[1:6] <- NA
    addWorksheet (wb,"metaData")
    writeData (wb,"metaData", meta_data, rowNames = F)
    saveWorkbook (wb, file.path(temp_dir, "qcrms_meta_data.xlsx"),
        overwrite = T)
    
    expect_warning(QCreport <- createQCreportObject(
        data_file="LCMS_xdata.Rdata", projectdir=temp_dir,
        metaData_file="qcrms_meta_data.xlsx", xcms_output="xdata",
        classLabel="sample_group", excludeQC="remove", msms_label="MSMS",
        plot_eic=FALSE,
        raw_path=system.file("cdf", package="faahKO"))
    )
    
    # Fails on travis, as apperantly packages can be installed in two different
    # locations.
    #Last 13 lines of output:
    # x[3]: "/tmp/RtmpY18EPf/RLIBS_3ab6462428bf/faahKO/cdf/KO/ko18.CDF"
    # y[3]: "/home/travis/R/Library/faahKO/cdf/KO/ko18.CDF"
    # expect_equivalent(QCreport$raw_paths, file.path(raw_paths))
    
    expect_equal(QCreport$metaData$file, "qcrms_meta_data.xlsx")

    expect_equal(QCreport$metaData$table[, -c(2,5)],
        structure(list(
            Sample=c("ko15", "ko16", "ko18", "ko19", "ko21", "ko22",
                "wt16", "wt18", "wt19", "wt21", "wt22"),
            sample_group=c("Removed", "QC", "QC", "QC", "QC", "QC", "WT", "WT",
                "WT", "WT", "WT"),
            remove=c(TRUE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
            injection_order=1:11, batch=c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                1L, 1L)),
            row.names=c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L),
            class="data.frame")
    ) 
    
    context ("locate-raw-files returns error if can't locate raw files")
    QCreport$raw_path <- system.file("cdf", "WT",package="faahKO")
    expect_error(qcrms:::locate_raw_files(QCreportObject=QCreport),
        regexp="Some or all raw data files couldn't be located in folder")
    
    context ("If different file types are located in the raw data folder
        return error.")
    dir.create(file.path(temp_dir, "raw_files"))
    file.create(file.path(temp_dir, "raw_files", "file1.mzML"))
    file.create(file.path(temp_dir, "raw_files", "file2.CDF"))
    QCreport$raw_path <- file.path(temp_dir, "raw_files")
    expect_error(qcrms:::locate_raw_files(QCreport),
        regexp="Some or all raw data files couldn't be located in folder")
    
    context ("LCMS: Create metadata file from sample filenames.")
    expect_warning(QCreport <- createQCreportObject(
        data_file="LCMS_xdata.Rdata", projectdir=temp_dir, xcms_output="xdata",
        classLabel="sample_group", excludeQC="remove", msms_label="MSMS",
        plot_eic=FALSE, group_names=c("ko", "wt"), Blank_label=NULL,
        QC_label=NULL)
    )
    
    expect_equal(QCreport$metaData$table[, -2], 
        structure(list(
            Sample = c("ko15", "ko16", "ko18", "ko19", "ko21", "ko22", "wt15",
                "wt16", "wt18", "wt19", "wt21", "wt22"),
            sample_group = c("ko", "ko", "ko", "ko", "ko", "ko", "wt", "wt",
                "wt", "wt", "wt", "wt"),
            injection_order = 1:12),
        row.names = c(NA, 12L),
        class = "data.frame")
    )
    
    unlink(file.path(temp_dir, "LCMS_xdata.Rdata"))
    unlink(file.path(temp_dir, "qcrms_test_meta_data_file.csv"))
    unlink(file.path(temp_dir, "LCMS_xdata.xlsx"))
    unlink(file.path(temp_dir, "LCMS_xdata.pdf"))
    unlink(file.path(temp_dir, "LCMS_xdata_EICs.pdf"))
    unlink(file.path(temp_dir, "qcrms_meta_data.xlsx"))
    unlink(file.path(temp_dir, "raw_files"), recursive=TRUE)    
})
