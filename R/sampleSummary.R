#' @importFrom openxlsx read.xlsx
#' @importFrom openxlsx getSheetNames
#' @importFrom dplyr left_join
#' @importFrom mzR openMSfile
#' @importFrom mzR tic
#' @importFrom mzR close
#' @importFrom utils read.table
#' @importFrom MSnbase fileNames
#'
NULL

#' Return file extension from character vector if file paths
#' @param file_paths character(), character vector containing file paths
#' @return character(), character vector with unique file extensions

get_file_extension <- function(file_paths) {
    extensions <- strsplit(file_paths, "\\.")
    extensions <- vapply(extensions, function(x) x[[length(x)]],
        vector(mode="character", 1))
    unique(paste0(".", extensions))
}

#' Locate raw LC/MS raw files on local files system and align with xcms output
#' 
#' @param QCreportObject Qcreport object

locate_raw_files <- function(QCreportObject){
    if (is(QCreportObject$xset, "xcmsSet")){
        file_extension <- get_file_extension(QCreportObject$xset@filepaths)
    } 
    else if (is(QCreportObject$xset, "XCMSnExp")){
        file_extension <-
            get_file_extension(MSnbase::fileNames(QCreportObject$xset))
    }
    
    if (length(grep(file_extension, QCreportObject$metaData$table$Sample)) == 0){
        raw_paths <- paste(QCreportObject$raw_path, "/",
        QCreportObject$metaData$table$Sample, file_extension, sep="")
    } else {
        raw_paths <- paste(QCreportObject$raw_path, "/",
        QCreportObject$metaData$table$Sample, sep="")
    }
    ## If files are located across multiple folders, locate them one-by-one
    if (!all(file.exists(raw_paths))) {
        files_to_locate <- paste0(row.names(QCreportObject$xset@phenoData),
            file_extension)
        raw_paths <- rep(NA, length(files_to_locate))
        for (file in seq_len(length(files_to_locate))) {
            file_path <- dir(QCreportObject$raw_path, recursive=TRUE,
                pattern=files_to_locate[file], full.names=TRUE)
            if (length(file_path) > 0) {
                raw_paths[file] <- file_path
            }
        }
    }
    if (any(is.na(raw_paths))) {
        stop("Some or all raw data files couldn't be located in folder
            specified in raw_path parameter.")
    }
    raw_paths
}

#' Generate meta data from sample filenames
#' 
#' @param QCreportObject Qcreport object

meta_data_from_filenames <- function(QCreportObject) {
    meta_data_table <- QCreportObject$metaData$table
    meta_data_table[ , QCreportObject$metaData$classColumn] <- "sample"
    
    class_labels <- c(QCreportObject$Blank_label, QCreportObject$QC_label,
        QCreportObject$group_names)
    
    if (length(class_labels) > 0L) {
        for (classl in class_labels) {
            ch <- grep(classl, meta_data_table$Sample)
            if (length(ch) > 0L) {
                meta_data_table[ch , QCreportObject$metaData$classColumn] <- 
                    classl
            }
        }
    }
    meta_data_table
}

#' Exctract and return ggplot2 object of EICs for specified xmcm features
#'
#' @param QCreportObject Qcreport object

sampleSummary <- function(QCreportObject) {
  if (!is.null(QCreportObject$xset)) {
    if (is(QCreportObject$xset, "xcmsSet")){
      QCreportObject$metaData$table <- data.frame(
          Sample=rownames(QCreportObject$xset@phenoData),
          xcms_class=QCreportObject$xset@phenoData$class,
          stringsAsFactors=FALSE)
    }
    else if (is(QCreportObject$xset, "XCMSnExp")){
      QCreportObject$metaData$table <- data.frame(
          Sample=rownames(pData(QCreportObject$xset)),
          xcms_class=pData(QCreportObject$xset)$sample_group,
          stringsAsFactors=FALSE)
    }
    if (length(unique(QCreportObject$metaData$table$xcms_class)) == 1L) {
      QCreportObject$metaData$table$xcms_class <- NULL
    }
  } else {
    QCreportObject$metaData$table <- data.frame(Sample=
      colnames(QCreportObject$peakMatrix), stringsAsFactors=FALSE)
  }

  if (!is.null(QCreportObject$metaData$file)) {
    if (grepl(".xls", tolower(QCreportObject$metaData$file)) ||
        tolower(tools::file_ext(QCreportObject$metaData$file)) %in%
        c("xls", "xlsx")) {
      if ("metaData" %in%
          openxlsx:: getSheetNames(QCreportObject$metaData$file)) {
        sheet <- "metaData"
      } else {
        sheet <- 1L
      }
      metaDataTable <- openxlsx::read.xlsx(
          xlsxFile=QCreportObject$metaData$file, sheet=sheet)
    } else {
      metaDataTable <- read.csv(QCreportObject$metaData$file, header=TRUE,
        check.names=FALSE, stringsAsFactors=FALSE)
    }
    QCreportObject$metaData$table <-
      dplyr::left_join(QCreportObject$metaData$table,
        metaDataTable, by="Sample")
  } else {
      QCreportObject$metaData$table <- meta_data_from_filenames(QCreportObject)
  }

  # Check that column in meta data selected to be used for class lables doesn't
  # contain NA's.
  # checks if ClassQC column exists, to use that for Blank and QC sample
  # indices.

  if (any(is.na(QCreportObject$metaData$
        table[, QCreportObject$metaData$classColumn])) &
        "ClassQC" %in% colnames(QCreportObject$metaData$table)) {
    na_hits <- which(is.na(QCreportObject$
      metaData$table[, QCreportObject$metaData$classColumn]))
    QCreportObject$metaData$table[na_hits,
      QCreportObject$metaData$classColumn] <-
      QCreportObject$metaData$table[na_hits, "ClassQC"]
    if (any(is.na(QCreportObject$
        metaData$table[, QCreportObject$metaData$classColumn]))) {
      stop("Selected meta data column for class labels contains missing
        values!")
    }
  } else if (any(is.na(QCreportObject$
      metaData$table[, QCreportObject$metaData$classColumn]))) {
    stop("Selected meta data column for class labels contains missing values!")
  }

  ## Remove MS/MS data files if these are present
  msms_sample_hits <- which(QCreportObject$metaData$table
    [, QCreportObject$metaData$classColumn] == QCreportObject$msms_label)
  if (length(msms_sample_hits) > 0L) {
    QCreportObject$peakMatrix <-
      QCreportObject$peakMatrix[, -c(msms_sample_hits)]
    QCreportObject$metaData$table <-
      QCreportObject$metaData$table[-c(msms_sample_hits), ]
    if (is(QCreportObject$xset, "xcmsSet")){
        # Phenodata
        QCreportObject$xset@phenoData <- QCreportObject$
        xset@phenoData[-c(msms_sample_hits), , drop=FALSE]
        # rt
        QCreportObject$xset@rt$raw[msms_sample_hits] <- NULL
        QCreportObject$xset@rt$corrected[msms_sample_hits] <- NULL
    }
    if (is(QCreportObject$xset, "XCMSnExp")){
        # Phenodata
        QCreportObject$xset@phenoData <- QCreportObject$
        xset@phenoData[-c(msms_sample_hits), , drop=FALSE]
        # rt, TODO, Check where I am using RT list
        #'xcms::rtime(QCreportObject$xset, adjusted=FALSE, 
        #'    bySample=TRUE)[msms_sample_hits] <- NULL
        #'QCreportObject$xset@rt$corrected[msms_sample_hits] <- NULL
    }
  }

  if (!is.null(QCreportObject$raw_path)) {
      QCreportObject$raw_paths <- locate_raw_files(QCreportObject)
  } else if (!is.null(QCreportObject$xset)) {
      if (is(QCreportObject$xset, "xcmsSet")){
          QCreportObject$raw_paths <-
              QCreportObject$xset@filepaths
      }
      else if(is(QCreportObject$xset, "XCMSnExp")){
          QCreportObject$raw_paths <-
              MSnbase::fileNames(QCreportObject$xset)
      }
      if (length(msms_sample_hits) > 0L) {
        QCreportObject$raw_paths <- QCreportObject$raw_paths[-msms_sample_hits]
      }
  }

  # TimeStamp from mzML file and file size

  QCreportObject$timestamps <- rep(NA, length(QCreportObject$raw_paths))
  QCreportObject$fileSize <- rep(NA, length(QCreportObject$raw_paths))

  # In case input is peak matrix, with no access to raw data files
  if (length(QCreportObject$timestamps) < 1L) {
    QCreportObject$timestamps <- rep(NA, ncol(QCreportObject$peakMatrix))
    QCreportObject$fileSize <- rep(NA, ncol(QCreportObject$peakMatrix))
  }

  if (!is.null(QCreportObject$raw_paths)) {
    for (i in seq_len(length(QCreportObject$raw_paths))) {
      if (file.exists(QCreportObject$raw_paths[i])) {
        QCreportObject$timestamps[i] <-
          mzML.startTimeStamp(filename=QCreportObject$raw_paths[i])
        # File size in MB
        QCreportObject$fileSize[i] <-
          file.info(QCreportObject$raw_paths[i])$size / (1024L * 1024L)
      }
    }
  }

  # TIC data
  QCreportObject$TICs <- rep(NA, nrow(QCreportObject$metaData$table))
  QCreportObject$TICraw <- rep(NA, nrow(QCreportObject$metaData$table))
  QCreportObject$TICdata <- vector("list", nrow(QCreportObject$metaData$table))

  if (!is.null(QCreportObject$xset)) {
    if (is(QCreportObject$xset, "xcmsSet")){
        peakt <- QCreportObject$xset@peaks
    }
    else if (is(QCreportObject$xset, "XCMSnExp")){
        peakt <- xcms::chromPeaks(QCreportObject$xset)
    }

    ## Remove MS/MS data files
    if (length(msms_sample_hits) > 0L) {
      sample_hits <- peakt[, "sample"] %in% msms_sample_hits
      peakt <- peakt[!sample_hits, ]
    }

    peaksDetected <- as.vector(table(peakt[, "sample"]))

    #TIC for extracted peaks
    QCreportObject$TICs <- tapply(peakt[, "into"], peakt[, "sample"], FUN=sum)

    # TIC's for raw files
    # mzR is about 10 times faster to read Raw data files than MSnbase
    # implementation, So for tic needs I am using it.
    for (sn in seq_len(length(QCreportObject$TICraw))) {
      if (file.exists(QCreportObject$raw_paths[sn])) {
        A <- mzR::openMSfile(QCreportObject$raw_paths[sn])
        tic <- tryCatch(mzR::tic(A), error=function(e) {return(NULL)})
        QCreportObject$TICraw[sn] <- sum(tic$TIC)
        QCreportObject$TICdata[[sn]] <- tic$TIC
        mzR::close(A)
        rm(A, tic)
      }
    }
  } else {
    peaksDetected <- as.vector(apply(QCreportObject$peakMatrix, 2L,
      function(x) length(which(!is.na(x)))))
    QCreportObject$TICs <- colSums(QCreportObject$peakMatrix,
      na.rm=TRUE, dims=1L)
    QCreportObject$TICraw <- colSums(QCreportObject$peakMatrix,
      na.rm=TRUE, dims=1L)
    QCreportObject$TICdata <- vector("list", ncol(QCreportObject$peakMatrix))
  }

  QCreportObject$metaData$samp_lab <- as.factor(QCreportObject$metaData$
    table[, QCreportObject$metaData$classColumn])

  QCreportObject$samp.sum <-
    data.frame(sample=QCreportObject$metaData$table$Sample,
      meas.time=as.character(QCreportObject$timestamps),
      class=as.character(QCreportObject$metaData$samp_lab),
      peaks.detected=peaksDetected,
      file.size=round(QCreportObject$fileSize, 2L))

  colnames(QCreportObject$samp.sum) <- c("Sample", "Measurement time",
    "Class", "Number of peaks", "mzML file size (MB)")

  QCreportObject$samp.sum <-
    QCreportObject$samp.sum[order(QCreportObject$timestamps), ]
  row.names(QCreportObject$samp.sum) <- NULL

  rNames <- c("Number of samples:", "Sample classes:")
  cValues <-  c((nrow(QCreportObject$metaData$table)),
    paste(unique(QCreportObject$metaData$
      table[, QCreportObject$metaData$classColumn]), collapse=", "))

  QCreportObject$projectHeader <- rbind(QCreportObject$projectHeader,
    c("", ""), cbind(rNames, cValues))

  # Reorder metaData and peak data according measurement order.
  QCreportObject$metaData$table$injection_order <-
    order(order(QCreportObject$timestamps))

  QCreportObject$peakMatrix <-
    QCreportObject$
      peakMatrix[, order(QCreportObject$metaData$table$injection_order)]
  QCreportObject$metaData$table <- QCreportObject$metaData$
    table[order(QCreportObject$metaData$table$injection_order), ]

  QCreportObject$QC_hits <-
    which(QCreportObject$metaData$table[, QCreportObject$metaData$
      classColumn] == QCreportObject$QC_label)
  QCreportObject$Blank_hits <- which(QCreportObject$metaData$
      table[, QCreportObject$metaData$classColumn] ==
        QCreportObject$Blank_label)

  QCreportObject$metaData$samp_lab <-
    QCreportObject$metaData$table[, QCreportObject$metaData$classColumn]

  QCreportObject
}
