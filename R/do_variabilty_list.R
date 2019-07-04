#' Calculate RSD\% or MAD values per sample group of input peakmatrix
#'
#' @param peak_data peak matrix
#' @param classes Vector of class labels
#' @return List of RSD\% or MAD values for each feature for each sample group
#' @export

do_variability_list <- function(peak_data, classes, method="RSD"){
  
  peak_data <- check_peak_matrix(peak_data = peak_data, classes = classes)
  
  cl <- unique (classes)
  out <- vector("list",length(cl))
  names (out) <- cl

  #Calculate RSD before scaling and MV imputation
  if (method=="RSD"){
    FUN <- function (x) sd(x,na.rm=T)/mean(x,na.rm=T)*100.0
  } else if (method=="MAD") {
    FUN <- function (x) mad(x, na.rm=T, constant = 1.4826)
  } else if (method=="median"){
    FUN <- function (x) median(x, na.rm = T)
  } else if (method=="none") {
    FUN <- NULL  
  }else {
    stop ("Method specified is not supported")
  }
  
  for (slab in 1: length(out)){
    if (method=="none"){
      out[[slab]] <- c(as.matrix(peak_data[,classes==names(out)[slab]]))
    } else {
      out[[slab]] <- apply(rbind(peak_data[,classes==names(out)[slab]], NULL),1,FUN)
    }
  }
  
  out$variability_method <- method
  out
}
