#' @import openxlsx
NULL

#' This function format ions identifiers
#' @author G. Le Corguille
#' @param variableMetadata vairable meta data object
#' @param numDigitsRT decimal digits for RT
#' @param numDigitsMZ number of deciaml digits for mz
#' @export

formatIonIdentifiers <- function(variableMetadata, numDigitsRT=0, numDigitsMZ=0) {
  splitDeco = strsplit(as.character(variableMetadata$name),"_")
  idsDeco = sapply(splitDeco, function(x) { deco=unlist(x)[2]; if (is.na(deco)) return ("") else return(paste0("_",deco)) })
  namecustom = make.unique(paste0("M",round(variableMetadata[,"mz"],numDigitsMZ),"T",round(variableMetadata[,"rt"],numDigitsRT),idsDeco))
  variableMetadata=cbind(name=variableMetadata$name, namecustom=namecustom, variableMetadata[,!(colnames(variableMetadata) %in% c("name"))])
  return(variableMetadata)
}


#' This function convert if it is required the Retention Time in minutes
#' @author G. Le Corguille
#' @param variableMetadata vairable meta data object
#' @param convertRTMinute convert RT in seconds to minutes
#' @export

RTSecondToMinute <- function(variableMetadata, convertRTMinute) {
  if (convertRTMinute){
    #converting the retention times (seconds) into minutes
    print("converting the retention times into minutes in the variableMetadata")
    variableMetadata[,"rt"]=variableMetadata[,"rt"]/60
    variableMetadata[,"rtmin"]=variableMetadata[,"rtmin"]/60
    variableMetadata[,"rtmax"]=variableMetadata[,"rtmax"]/60
  }
  return (variableMetadata)
}

#' This function prepares peaklist object from xcms output
#' @author G. Le Corguille
#' @param xset XCMS object to process
#' @param intval Which value to use for peak area, see XCMS manual for more details
#' @param convertRTMinute If RT shoudl be converted from seconds to minutes
#' @param numDigitsRT decimal digits for RT
#' @param numDigitsMZ number of deciaml digits for mz
#' @export



#@Modified from W4M code by G. Le Corguille
# value: intensity values to be used into, maxo or intb
getPeaklistW4M <- function(xset, intval="into",convertRTMinute=F,numDigitsMZ=4,numDigitsRT=0) {
  variableMetadata_dataMatrix <- xcms::peakTable(xset, method="medret", value=intval)
  variableMetadata_dataMatrix = cbind(name=groupnames(xset),variableMetadata_dataMatrix)
  
  # dataMatrix is the same as peakMatrix in QCreport object
  #dataMatrix = variableMetadata_dataMatrix[,(make.names(colnames(variableMetadata_dataMatrix)) %in% c("name", make.names(sampnames(xset))))]

  variableMetadata = variableMetadata_dataMatrix[,!(make.names(colnames(variableMetadata_dataMatrix)) %in% c(make.names(sampnames(xset))))]
  variableMetadata = RTSecondToMinute(variableMetadata, convertRTMinute)
  variableMetadata = formatIonIdentifiers(variableMetadata, numDigitsRT=numDigitsRT, numDigitsMZ=numDigitsMZ)

  out <- list()
  #out$dataMatrix <- dataMatrix
  out$variableMetaData <- variableMetadata
  out
}

#' Create xlsx output file from XCMS output
#'
#' @param QCreportObject Qcreport object
#' @export

createXlsx <- function (QCreportObject)
{

  if (!is.null(QCreportObject$xset)){
    W4M <- getPeaklistW4M(xset=QCreportObject$xset)
    rownames(QCreportObject$peakMatrix) <- W4M$variableMetaData$name
 
  } else {
    #QCreportObject$data$dataMatrix = as.matrix(read.table(QCreportObject$data_file, header=TRUE, row.names=1, check.names=FALSE))
    QCreportObject$peakMatrix[QCreportObject$peakMatrix == 0] <= NA
  }
  
  # Write out meta data
  # Columns sample_name, class, injection order

  #QCreportObject$metaData$table$injection_order <- order(order(QCreportObject$timestamps))
  
  # Put label "Removed" for qc samples used as leading/outliers during processing in meta data column used for class label
  QCreportObject$metaData$table[,QCreportObject$metaData$classColumn] <- QCreportObject$metaData$samp_lab
  
  # W4M for no good reason decided to check if file names contains "-" and similar symbols. Why?
  # If it xcms can handle original file names, and after al files have been processed.
  # Where is tracability to the original files if this has been done to outputs?

  if (!all(colnames(QCreportObject$data$dataMatrix) == QCreportObject$metaData$table$Sample)){
    
    if (all(colnames(QCreportObject$data$dataMatrix)==make.names(QCreportObject$metaData$table$Sample))){
      colnames(QCreportObject$data$dataMatrix) <- QCreportObject$metaData$table$Sample
    } else {
        stop ("Sample names present in XCMS output doesn't match with meta data.")
      }
  }

  wb <- createWorkbook()
  addWorksheet (wb,"dataMatrix")
  writeData (wb,"dataMatrix", QCreportObject$peakMatrix, rowNames = T)

  addWorksheet (wb, "metaData")
  writeData (wb, "metaData", QCreportObject$metaData$table, rowNames = F)
  if (!is.null(QCreportObject$xset))
  {
    addWorksheet (wb, "variableMetaData")
    writeData (wb, "variableMetaData", W4M$variableMetaData, rowNames = F)
  } else
  {
    addWorksheet (wb, "variableMetaData")
    variableMetaData = rowMeans(QCreportObject$data$dataMatrix, na.rm = TRUE, dims = 1)
    print(data.frame(mz=names(variableMetaData), intensity=as.vector(variableMetaData)))
    writeData (wb, "variableMetaData", data.frame(mz=names(variableMetaData), intensity=as.vector(variableMetaData)), rowNames = F)
  }  
  
  saveWorkbook (wb, QCreportObject$xlsxout, overwrite = T)

  QCreportObject
}

