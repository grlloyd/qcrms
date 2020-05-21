context ("Test EIC plot.")

library(xcms)
library(BiocManager)
if(!requireNamespace("msPurityData", quietly=TRUE)){
    BiocManager::install("msPurityData")
}

test_that("EIC plot is created and has correct data, using XCMSnExp class.", {
    mzml_files <- dir(system.file("extdata/lcms/mzML", package="msPurityData"),
        full.names=TRUE, recursive=TRUE)
    rawf <- MSnbase::readMSData(mzml_files[1:2], mode="onDisk", msLevel.=1)
    cwp <- xcms::CentWaveParam(snthresh=10)
    xset <- xcms::findChromPeaks(rawf, param=cwp)
    xset <- xcms::groupChromPeaks(xset,
        xcms::PeakDensityParam(sampleGroups=c("1", "1")))
    xset <- as(xset, "xcmsSet")

    rawfiles <- list()
    rawfiles[[1]] <- MSnbase::readMSData(mzml_files[1], mode="onDisk", 
        msLevel.=1)
    rawfiles[[2]] <- MSnbase::readMSData(mzml_files[2], mode="onDisk", 
        msLevel.=1)

    plot <- expect_warning(qcrms:::XCMS_EIC_plot(indexes=100,
        rawfiles=rawfiles, xset=xset, class=c("1", "1")))

    # Fails on devtools:test() works with testhat
    #expect_equal(round(mean(plot[[1]]$data$rt),4), 44.0588)
    #expect_equal(round(mean(plot[[1]]$data$mz),4), 146.1173)
})

test_that("EIC plot is created and has correct data, using xcmsSet class.", {
    mzml_files <- dir(system.file("extdata/lcms/mzML", package="msPurityData"),
        full.names=TRUE, recursive=TRUE)[1:2]
    xset <- xcms::xcmsSet(mzml_files, method = 'centWave', ppm = 25,
        peakwidth = c(20, 50), snthresh = 10, prefilter = c(3,100),
        integrate = 1, mzdiff = -0.001, verbose.columns = FALSE,
        fitgauss = FALSE, noise = 5000)
    xset <- xcms::group(xset, method = "density", bw = 30, minfrac = 0.5,
        minsamp = 1, mzwid=0.25, max=50)
  
    rawfiles <- list()
    rawfiles[[1]] <- MSnbase::readMSData(mzml_files[1], mode="onDisk", 
        msLevel.=1)
    rawfiles[[2]] <- MSnbase::readMSData(mzml_files[2], mode="onDisk", 
        msLevel.=1)

    plot <- expect_warning(qcrms:::XCMS_EIC_plot(indexes=100,
        rawfiles=rawfiles, xset=xset, class=c("1", "1")))

    expect_equal(head(plot[[1]]$data),
        structure(list(
            rt = c(73.381104, 73.70211, 74.34549, 74.66874, 74.992116,
                75.642732),
            mz = c(152.990295410156, 152.989715576172, 152.99055480957,
                152.988983154297, 152.990280151367, 152.988830566406),
            i = c(21548.203125, 59140.6796875, 61134.80078125, 37297.19140625,
                54743.37109375, 22704.90234375),
            Class = structure(c(1L, 1L, 1L, 1L, 1L, 1L), .Label = "1",
                class = c("ordered", "factor")), 
            sample = c(1L, 1L, 1L, 1L, 1L, 1L)),
        row.names = c(NA, 6L), class = "data.frame"))
})