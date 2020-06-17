context ("Test start time stamp extraction from mzML files.")

library(xcms)
library(BiocManager)

if(!requireNamespace("faahKO", quietly=TRUE)){
    BiocManager::install("faahKO")
}

test_that("Timestamp gets extracted", {
    mzml_files <- dir(system.file("extdata/MTBLS404/mzML",
        package = "qcrmsData"), full.names = TRUE, recursive = TRUE)
    time_stamp <-
        qcrms:::mzML.startTimeStamp(
            mzml_files[grep("Blanc04.mzML", mzml_files)])
    expect_equal(time_stamp, "2009-05-07 21:43:00Z")
})

test_that("Warninn message is created if files are not mzML", {
    mzml_files <- dir(system.file("cdf", package = "faahKO"),
        full.names = TRUE, recursive = TRUE)
    expect_message(expect_warning(time_stamp <-
        qcrms:::mzML.startTimeStamp(mzml_files[1])))
})

test_that ("If run block doesn't contain timeStamp, function stops", {
    mzml_files <- dir(system.file("extdata/MTBLS404/mzML",
        package = "qcrmsData"), full.names = TRUE, recursive = TRUE)
    con <- file(mzml_files[grep("Blanc04.mzML", mzml_files)], "r")
    mzml_content <- readLines(con)
    mzml_content[90] <- sub (" startTimeStamp=\"2009-05-07T21:43:00Z\"", "",
        mzml_content[90])
    close (con)
    output_file <- file.path(tempdir(), "out.mzML")
    con <- file(output_file, "w")
    writeLines(con, text=mzml_content)
    close (con)
    time_stamp <- qcrms:::mzML.startTimeStamp(output_file)
    expect_true(is.na(time_stamp))
    file.remove(output_file)
})
