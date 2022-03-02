#' @title reading PV2 header content
#' @description reads the content of the header PV2
#' @param data hl7 content
#' @importFrom stringr str_split
#' @importFrom stringr fixed
get_PV2 <- function(data){ ####
  if(missing(data)){
    stop('data is missing in .get_PV2')
  }
  term <- 'PV2'
  result <- list()
  temp <- get_hl7_line(data, term)
  temp <- str_split(temp, fixed('|'), simplify = T)
  if(!is.na(temp[1])){if(!nchar(temp[1])==0)
  {result$FELD <- temp[1]}}
  if(!is.na(temp[2])){if(!nchar(temp[2])==0)
  {result$PRIOR_PENDING_LOCATION <- temp[2]}}
  if(!is.na(temp[3])){if(!nchar(temp[3])==0)
  {result$ACCOMMODATION_TYPE <- temp[3]}}
  if(!is.na(temp[4])){if(!nchar(temp[4])==0){
    ADMISSION_REASON <- str_split(temp[4],fixed('^'), simplify = T)
    if(!is.na(ADMISSION_REASON[1]))
    {if(!nchar(ADMISSION_REASON[1])==0)
    {result$ID <- ADMISSION_REASON[1]}}
    if(!is.na(ADMISSION_REASON[2]))
    {if(!nchar(ADMISSION_REASON[2])==0)
    {result$TEXT <- ADMISSION_REASON[2]}}
  }}
  if(!length(result) == 0){
    return(result)
  }else{
    return(NULL)
  }
}
