#' Extract measurement time stamp fro mzML data file
#'
#' @param filename Name of the mzML file
#' @export

mzML.startTimeStamp <- function(filename){

  # XML R package has a bug as it doesn't release memory after processing a file. So for now default code is to parse XML as the text file.
  # If the first 5 lines of the file don't have reference to mzML schema - stop
  con = file(filename, "r")
  line = readLines(con, n=5)
  if (length(grep ("mzML",line))==0)
  {
   cat ("Measurement date and time information can be extracted only from mzML data files.. \n")
   startTimeStamp <- NA
   #stop ()
  } else
  {
    while ( TRUE ) {
      line = readLines(con, n = 1)
         if ( length(grep("startTimeStamp",line)) == 1 ) {
         line <- strsplit(line,"startTimeStamp=\\\"")[[1]][2]
         startTimeStamp <- strsplit (line,"\\\"")[[1]][1]
         break
        }
      }
  }
  close(con)
  startTimeStamp <- sub ("T"," ",startTimeStamp)
	startTimeStamp
}

