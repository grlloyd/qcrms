context ("Test start time stamp extraction from mzML files.")

mzml_files <- dir(system.file("extdata/dims/mzML", package = "msPurityData"),
    full.names = TRUE, recursive = TRUE)

time_stamp <- qcrms:::mzML.startTimeStamp(mzml_files[1])

expect_equal(time_stamp, "2015-07-10 15:25:01Z")
