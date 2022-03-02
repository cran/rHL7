#' @title reading MRG header content
#' @description reads the content of the header MRG
#' @param data hl7 content
#' @importFrom stringr str_split
#' @importFrom stringr fixed
get_MRG <- function(data){ ####
  if(missing(data)){
    stop('data is missing in .get_MRG')
  }
  term <- 'MRG'
  result <- list()
  temp <- get_hl7_line(data, term)
  temp <- str_split(temp, fixed('|'), simplify = T)
  if(!is.na(temp[1])){if(!nchar(temp[1])==0)
  {result$FELD <- temp[1]}}
  if(!is.na(temp[2])){if(!nchar(temp[2])==0)
  {result$PRIOR_PATIENT_ID <- temp[2]}}
  if(!is.na(temp[3])){if(!nchar(temp[3])==0)
  {result$PRIOR_ALTERNATIVE_PATIENT_ID <- temp[3]}}
  if(!is.na(temp[4])){if(!nchar(temp[4])==0)
  {result$PRIOR_PATIENT_ACCOUNT_NUMBER <- temp[4]}}
  if(!is.na(temp[5])){if(!nchar(temp[5])==0)
  {result$PRIOR_EXTERNE_PATIENT_ID <- temp[5]}}
  if(!is.na(temp[6])){if(!nchar(temp[6])==0)
  {result$PRIOR_VISIT_NUMBER <- temp[6]}}
  if(!is.na(temp[4])){if(!nchar(temp[4])==0)
  {result$PRIOR_ALTERNATIVE_VISIT_ID <- temp[4]}}
  if(!length(result) == 0){
    return(result)
  }else{
    return(NULL)
  }
}
