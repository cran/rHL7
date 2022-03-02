#' @title extracts the content of the hl7 file
#' @description reads either file or just the content of the file and returns a list of headers. The input shall be either file or data.
#' @param file path of the hl7 file
#' @param data content of the hl7 file
# This function gets the data from all the lines of HL7 file
# test protocol included
#'
get_hl7_data <- function(file, data){
  if(!missing(file)){
    hl7_data <- readLines(file, encoding = "latin1")
  }
  if(!missing(data)){
    hl7_data <- data
  }
  if(isempty(hl7_data)){
    return(NULL)
  }else{
    result <- list()
    result$msh   <- get_MSH(hl7_data)
    result$event <- get_EVN(hl7_data)
    result$pid   <- get_PID(hl7_data)
    result$pv1   <- get_PV1(hl7_data)
    result$pv2   <- get_PV2(hl7_data)
    result$mrg   <- get_MRG(hl7_data)
    if(!length(result) == 0){
      return(result)
    }else{
      return(NULL)
    }
  }
}
