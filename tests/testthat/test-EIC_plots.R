context ("Test EIC plot.")

library(xcms)
library(BiocManager)
if(!requireNamespace("faahKO", quietly=TRUE)){
    BiocManager::install("faahKO")
}

xcms_groups_rt_mz_summary_output <- 
  list(min_rt = structure(c(3628.145, 3640.664), .Dim = 1:2,
    .Dimnames = list("FT100", c("ko15.CDF", "ko16.CDF"))),
  max_rt = structure(c(3650.054, 3684.483),.Dim = 1:2,
    .Dimnames = list("FT100", c("ko15.CDF", "ko16.CDF"))),
  min_mz = structure(c(321.100006103516, 321.100006103516), .Dim = 1:2,
  .Dimnames = list("FT100", c("ko15.CDF", "ko16.CDF"))),
  max_mz = structure(c(321.100006103516, 321.100006103516), .Dim = 1:2,
    .Dimnames = list("FT100", c("ko15.CDF", "ko16.CDF")))
)

plot_data_no_rt <- structure(list(
  rt = c(3628.145, 3629.71, 3631.274, 3639.099, 3640.664, 3642.229),
  mz = c(321.100006103516, 321.100006103516, 321.100006103516,
    321.100006103516, 321.100006103516, 321.100006103516),
  i = c(710, 762, 769, 3054, 4254, 5381),
  Class = structure(c(1L, 1L, 1L, 1L, 1L, 1L), .Label = "1",
    class = c("ordered", "factor")),
  sample = c(1L, 1L, 1L, 1L, 1L, 1L)),
  row.names = c(NA, 6L), class = "data.frame"
)
  
plot_data_rt <- structure(list(
  rt = c(3645.65283203125, 3647.27270507812, 3648.89086914062, 
    3656.9775390625, 3658.5922851562),
  mz = c(321.100006103516, 321.100006103516, 321.100006103516, 
    321.100006103516, 321.100006103516),
  i = c(710, 762, 769, 3054, 4254),
  Class = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "1",
    class = c("ordered", "factor")),
  sample = c(1L, 1L, 1L, 1L, 1L)),
  row.names = c(NA, 5L), class = "data.frame"
)

test_that("EIC plot is created and has correct data, using XCMSnExp class.", {
    mzml_files <- dir(system.file("cdf/KO", package="faahKO"),
        full.names=TRUE, recursive=TRUE)
    rawf <- MSnbase::readMSData(mzml_files[1:2], mode="onDisk", msLevel.=1)
    cwp <- xcms::CentWaveParam(snthresh=10, noise=5000)
    xset <- xcms::findChromPeaks(rawf, param=cwp)
    xset <- xcms::groupChromPeaks(xset,
        xcms::PeakDensityParam(sampleGroups=c("mzML", "mzML")))
    
    expect_equal(pData(xset),
        structure(list(sampleNames = c("ko15.CDF", "ko16.CDF")),
        row.names = c("ko15.CDF", "ko16.CDF"), class = "data.frame"))
    
    expect_equal(pData(rawf),
        structure(list(sampleNames = c("ko15.CDF", "ko16.CDF")),
        row.names = c("ko15.CDF", "ko16.CDF"), class = "data.frame"))
    
    rawfiles <- list()
    rawfiles[[1]] <- MSnbase::readMSData(mzml_files[1], mode="inMemory", 
        msLevel.=1)
    rawfiles[[2]] <- MSnbase::readMSData(mzml_files[2], mode="inMemory", 
        msLevel.=1)

    expect_false(qcrms:::check_rt_correction(xset))
    
    expect_equal(qcrms:::xcms_groups_rt_mz_summary(xset=xset, indexes=100),
        xcms_groups_rt_mz_summary_output
    )
    
    plot_XCMSnEXP <- expect_warning(qcrms:::XCMS_EIC_plot(indexes=100,
        rawfiles=rawfiles, xset=xset, class=c("1", "1")))
    
    expect_equal(head(plot_XCMSnEXP[[1]]$data), plot_data_no_rt)
    
    context ("EIC on RT corrected data, XCMSnExp")
    
    xset <- xcms::adjustRtime(xset, param=ObiwarpParam(binSize=1, center=2))
    expect_true(qcrms:::check_rt_correction(xset))
    xset <- xcms::groupChromPeaks(xset,
        xcms::PeakDensityParam(sampleGroups=c("mzML", "mzML")))
    plot_XCMSnEXP_RT <- expect_warning(qcrms:::XCMS_EIC_plot(indexes = 100,
        rawfiles = rawfiles, xset = xset, class = c("1", "1")))
    
    expect_equal(plot_XCMSnEXP_RT[[1]]$data[1:5, ], plot_data_rt)
    
    rm (xset)

})

test_that("EIC plot is created and has correct data, using xcmsSet class.", {
    mzml_files <- dir(system.file("cdf/KO", package="faahKO"),
        full.names=TRUE, recursive=TRUE)[1:2]
    xset <- xcms::xcmsSet(mzml_files, method = 'centWave', ppm = 25,
        peakwidth = c(20, 50), snthresh = 10, prefilter = c(3,100),
        integrate = 1, mzdiff = -0.001, verbose.columns = FALSE,
        fitgauss = FALSE, noise = 5000)
    xset <- xcms::group(xset, method = "density", bw = 30, minfrac = 0.5,
        minsamp = 1, mzwid=0.25, max=50)
  
    rawfiles <- list()
    rawfiles[[1]] <- MSnbase::readMSData(mzml_files[1], mode="inMemory", 
        msLevel.=1)
    rawfiles[[2]] <- MSnbase::readMSData(mzml_files[2], mode="inMemory", 
        msLevel.=1)

    expect_false(qcrms:::check_rt_correction(xset))
    
    expect_equal(qcrms:::xcms_groups_rt_mz_summary(xset = xset, indexes = 100),
        list(min_rt = structure(c(3628.145, 3640.664), .Dim = 1:2,
          .Dimnames = list("321.1/3656", c("ko15", "ko16"))), 
        max_rt = structure(c(3650.054, 3684.483), .Dim = 1:2,
          .Dimnames = list("321.1/3656", c("ko15", "ko16"))),
        min_mz = structure(c(321.100006103516, 321.100006103516), .Dim = 1:2,
          .Dimnames = list("321.1/3656", c("ko15", "ko16"))),
        max_mz = structure(c(321.100006103516, 321.100006103516), .Dim = 1:2,
          .Dimnames = list("321.1/3656", c("ko15", "ko16"))))
    )
    
    plot_xcmsSet <- expect_warning(qcrms:::XCMS_EIC_plot(indexes=100, rawfiles=rawfiles,
        xset=xset, class=c("1", "1")))
    
    expect_equal(head(plot_xcmsSet[[1]]$data), plot_data_no_rt)
    
    context ("EIC on RT corrected data")
    
    xset <- xcms::retcor(object = xset, method = "obiwarp", center=2)
    expect_true(qcrms:::check_rt_correction(xset))
    xset <- xcms::group(xset, method = "density", bw = 30, minfrac = 0.5,
        minsamp = 1, mzwid=0.25, max=50)
    plot_xcmsSet_RT <- expect_warning(qcrms:::XCMS_EIC_plot(indexes = 100,
        rawfiles = rawfiles, xset = xset, class = c("1", "1")))
    
    expect_equal(plot_xcmsSet_RT[[1]]$data[1:5, ],
      structure(list(
        rt = c(3647.27270507812, 3648.89086914062, 3656.9775390625, 
          3658.59228515625, 3660.20629882812),
        mz = c(321.100006103516, 321.100006103516, 321.100006103516,
          321.100006103516, 321.100006103516),
        i = c(762, 769, 3054, 4254, 5381),
        Class = structure(c(1L, 1L, 1L, 1L, 1L), .Label = "1",
          class = c("ordered", "factor")), 
        sample = c(1L, 1L, 1L, 1L, 1L)), row.names = c(NA, 5L),
      class = "data.frame"))
})
