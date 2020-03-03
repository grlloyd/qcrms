#' Extract measurement time stamp fro mzML data file
#'
#' @param filename Name of the mzML file
#' @export

mzML.startTimeStamp <- function(filename) {

  # XML R package has a bug as it doesn't release memory after processing a
  # file. So for now default code is to parse XML as the text file.
  # If the first 5 lines of the file don't have reference to mzML schema - stop
  con <- file(filename, "r")
  line <- readLines(con, n=5L)
  if (length(grep("mzML", line)) == 0L) {
   cat("Measurement date and time information can be extracted only from mzML
    data files.. \n")
   startTimeStamp <- NA
  } else {
    while (TRUE) {
      line <- readLines(con, n=1L)
         if (length(grep("startTimeStamp", line)) == 1L) {
          line <- strsplit(line, "startTimeStamp=\\\"")[[1L]][2L]
          startTimeStamp <- strsplit(line, "\\\"")[[1L]][1L]
          break
         }
         if (length(grep("<spectrumList", line)) == 1L) {
          startTimeStamp <- NA
          break
         }
    }
  }
  close(con)
  startTimeStamp <- sub("T", " ", startTimeStamp)
  startTimeStamp
}
