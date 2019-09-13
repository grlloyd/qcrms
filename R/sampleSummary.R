#' @import xcms
#' @import openxlsx
#' @importFrom dplyr left_join
#' @importFrom  mzR openMSfile
#' @importFrom  mzR tic
#' @importFrom  mzR close
#'
NULL

#' Exctract and return ggplot2 object of EICs for specified xmcm features
#'
#' @param QCreportObject Qcreport object
#' @export


sampleSummary <- function (QCreportObject)
{
  
  if (!is.null(QCreportObject$xset)){
    
    QCreportObject$metaData$table <- data.frame(Sample=rownames(QCreportObject$xset@phenoData),
      xcms_class=QCreportObject$xset@phenoData$class)
    if (length(unique(QCreportObject$metaData$table$xcms_class)) == 1){
      QCreportObject$metaData$table$xcms_class <- NULL
    }
  } else {
    QCreportObject$metaData$table <- data.frame(Sample=colnames(QCreportObject$peakMatrix))
  }
  
  if (!is.null(QCreportObject$metaData$file)){
    if (grepl(".xls", tolower(QCreportObject$metaData$file)) || tolower(tools::file_ext(QCreportObject$metaData$file)) %in% c("xls", "xlsx")){
      if ("metaData" %in% getSheetNames(QCreportObject$metaData$file)){
        sheet = "metaData"
      } else {
        sheet = 1
      }
      metaDataTable <- read.xlsx(xlsxFile = QCreportObject$metaData$file, sheet=sheet)
    } else {
      metaDataTable <- read.table(QCreportObject$metaData$file, header=TRUE, check.names=FALSE)
    }
    
    if (!"Sample" %in% colnames(QCreportObject$metaData$table)){
      QCreportObject$metaData$table$Sample = rownames(QCreportObject$metaData$table)  
    }
    
    QCreportObject$metaData$table <- dplyr::left_join(QCreportObject$metaData$table, metaDataTable, by="Sample")
    
  } else {
    colnames(QCreportObject$metaData$table) <- QCreportObject$metaData$classColumn
    QCreportObject$metaData$table[,1] <- as.vector(QCreportObject$metaData$table[,1])
    QCreportObject$metaData$table$Sample <- rownames(QCreportObject$metaData$table)

    if (!is.null(QCreportObject$Blank_label)){
        bh <- grep(QCreportObject$Blank_label, rownames(QCreportObject$metaData$table))
        if (length(bh)>0) QCreportObject$metaData$table[bh,1] <- QCreportObject$Blank_label
      }
    if (!is.null(QCreportObject$QC_label)){
        qh <- grep(QCreportObject$QC_label, rownames(QCreportObject$metaData$table))
        if (length(qh)>0) QCreportObject$metaData$table[qh,1] <- QCreportObject$QC_label
      }
  }

  # Check that column in meta data selected to be used for class lables doesn't contain NA's
  # Also checks if ClassQC column exists, to use that for Blank and QC sample indices.
  
  if (any(is.na(QCreportObject$metaData$table[,QCreportObject$metaData$classColumn])) &
        "ClassQC" %in% colnames(QCreportObject$metaData$table)){
    na_hits <- which(is.na(QCreportObject$metaData$table[, QCreportObject$metaData$classColumn]))
    QCreportObject$metaData$table[na_hits, QCreportObject$metaData$classColumn] <- 
      QCreportObject$metaData$table[na_hits, "ClassQC"]
    if (any(is.na(QCreportObject$metaData$table[,QCreportObject$metaData$classColumn]))){
      stop("Selected meta data column for class labels contains missing values!")
    }
  } else if (any(is.na(QCreportObject$metaData$table[,QCreportObject$metaData$classColumn]))) {
    stop("Selected meta data column for class labels contains missing values!")
  }
  
  if (!is.null(QCreportObject$raw_path)){
    QCreportObject$raw_paths <- paste(QCreportObject$raw_path,"/",QCreportObject$metaData$table$Sample,".mzML",sep="")
    
    ## If files are located across multiple folders, locate them one-by-one
    if (!all(file.exists(QCreportObject$raw_paths))){
      files_to_locate <- paste(row.names(QCreportObject$xset@phenoData), "mzML", sep=".")
      for (file in 1:length(files_to_locate)){
        
        QCreportObject$raw_paths[file] <- dir (QCreportObject$raw_path, recursive = T, pattern = files_to_locate[file], full.names = T)
      }
    }
    
    if (all(is.na(QCreportObject$raw_paths))){
      stop ("Raw data files couldn't be located in folder specifies with raw_path parameter.")
    }
    
  } else if (!is.null(QCreportObject$xset)){
    QCreportObject$raw_paths <- QCreportObject$xset@filepaths
  }
  
  # TimeStamp from mzML file and file size
  
  QCreportObject$timestamps <- rep(NA, length(QCreportObject$raw_paths))
  QCreportObject$fileSize <- rep(NA, length(QCreportObject$raw_paths))
  
  # In case input is peak matrix, with no access to raw data files
  if (length(QCreportObject$timestamps) < 1){
    QCreportObject$timestamps <- rep(NA, ncol(QCreportObject$peakMatrix))
    QCreportObject$fileSize <- rep(NA, ncol(QCreportObject$peakMatrix))
  }
  
  if (!is.null(QCreportObject$raw_paths)){
    for (i in 1:length(QCreportObject$raw_paths)){
    
      if(file.exists(QCreportObject$raw_paths[i])){
      
        QCreportObject$timestamps[i] <- qcrms::mzML.startTimeStamp(filename=QCreportObject$raw_paths[i])
        # File size in MB
        QCreportObject$fileSize[i] <- file.info(QCreportObject$raw_paths[i])$size / (1024 * 1024)
      }
    }
  }
  
  #chit <- which(colnames(QCreportObject$metaData$table)==QCreportObject$metaData$classColumn)

  # TIC data
  QCreportObject$TICs <- rep(NA, nrow(QCreportObject$metaData$table))
  QCreportObject$TICraw <- rep(NA, nrow(QCreportObject$metaData$table))
  QCreportObject$TICdata <- vector("list", nrow(QCreportObject$metaData$table))

  if (!is.null(QCreportObject$xset)){
    
    peakt <- QCreportObject$xset@peaks
    
    peaksDetected <- as.vector(table(peakt[,"sample"]))
    
    #TIC for extracted peaks
    QCreportObject$TICs <- tapply (peakt[,"into"], peakt[, "sample"], FUN=sum)
  
    # TIC's for raw files
    # mzR is about 10 times faster to read Raw data files than MSnbase implementation, So for tic needs I am using it.
    for (sn in 1:length(QCreportObject$TICraw)){
      
      if (file.exists(QCreportObject$raw_paths[sn])){
        
        A <- mzR::openMSfile(QCreportObject$raw_paths[sn])
        tic <- tryCatch( mzR::tic(A), error = function(e){ return(NULL)})
        QCreportObject$TICraw[sn] <- sum(tic$TIC)
        QCreportObject$TICdata[[sn]] <- tic$TIC
        mzR::close(A)
        rm (A, tic)
      }
    }
  } else {
    peaksDetected = as.vector(apply(QCreportObject$peakMatrix, 2, function(x) length(which(!is.na(x)))))
    QCreportObject$TICs = colSums(QCreportObject$peakMatrix, na.rm = TRUE, dims = 1)
    QCreportObject$TICraw = colSums(QCreportObject$peakMatrix, na.rm = TRUE, dims = 1)
    QCreportObject$TICdata = vector("list", ncol(QCreportObject$peakMatrix))
  }
  
  QCreportObject$metaData$samp_lab <- as.factor (QCreportObject$metaData$table[,QCreportObject$metaData$classColumn])

  QCreportObject$samp.sum <- data.frame(sample=QCreportObject$metaData$table$Sample, 
                                        meas.time=QCreportObject$timestamps,
                                        class=QCreportObject$metaData$samp_lab,
                                        peaks.detected=peaksDetected,
                                        file.size=round(QCreportObject$fileSize, 2))

  colnames (QCreportObject$samp.sum) <- c("Sample", "Measurement time",
    "Class", "Number of peaks", "mzML file size (MB)")

  QCreportObject$samp.sum <- QCreportObject$samp.sum[order(QCreportObject$timestamps),]
  
  row.names(QCreportObject$samp.sum) <- NULL

  rNames <- c("Number of samples:", "Sample classes:")
  cValues <-  c((nrow(QCreportObject$metaData$table)),
              paste(unique(QCreportObject$metaData$table[, QCreportObject$metaData$classColumn]), collapse=", "))

  QCreportObject$projectHeader <- rbind(QCreportObject$projectHeader,c("",""),cbind (rNames, cValues))

  # Reorder metaData and peak data according measurement order.
  QCreportObject$metaData$table$injection_order <- order(order(QCreportObject$timestamps))
  
  QCreportObject$peakMatrix <- QCreportObject$peakMatrix[,order(QCreportObject$metaData$table$injection_order)]
  QCreportObject$metaData$table <- QCreportObject$metaData$table[order(QCreportObject$metaData$table$injection_order),]
  
  QCreportObject$QC_hits <- which(QCreportObject$metaData$table[, QCreportObject$metaData$classColumn]==QCreportObject$QC_label)
  QCreportObject$Blank_hits <- which(QCreportObject$metaData$table[, QCreportObject$metaData$classColumn]==QCreportObject$Blank_label)
  
  QCreportObject$metaData$samp_lab <- QCreportObject$metaData$table[, QCreportObject$metaData$classColumn]
  
  QCreportObject
}
