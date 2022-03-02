#' @title reading EVN header content
#' @description reads the content of the header EVENT
#' @param data hl7 content
#' @importFrom stringr str_split
#' @importFrom stringr fixed
get_EVN <- function(data){ ####
  if(missing(data)){
    stop("data is missing for .get_EVN")
  }
  result <- list()
  term <- 'EVN'
  temp <- get_hl7_line(data, term)
  temp <- str_split(temp, fixed('|'), simplify = T)
  if(!is.na(temp[1])){if(!nchar(temp[1])==0)
  {result$FELD <- temp[1]}}
  if(!is.na(temp[2])){if(!nchar(temp[2])==0)
  {result$EVENT_ID <- temp[2]}}
  if(!is.na(temp[3])){if(!nchar(temp[3])==0){
    DATE_TIME <- get_date_time(temp[3])
    result$DATE <- DATE_TIME$DATE
    result$TIME <- DATE_TIME$TIME
  }}
  if(!is.na(temp[4])){if(!nchar(temp[4])==0)
  {result$EVENT_REASON_CODE <- temp[4]}}
  if(!is.na(temp[5])){if(!nchar(temp[5])==0)
  {result$OPERATOR_ID <- temp[5]}}
  if(!is.na(temp[6])){if(!nchar(temp[6])==0)
  {result$EVENT_OCCURRED <- temp[6]}}
  if(!length(result) == 0){
    return(result)
  }else{
    return(NULL)
  }
}
