#' @title reading MSH header content
#' @description reads the content of the header MSH
#' @param data hl7 content
#' @importFrom stringr str_split
#' @importFrom stringr fixed
get_MSH <-  function(data){ ####
  if(missing(data)){
    stop("data is missing for .get_MSH")
  }
  term <-  'MSH'
  result <-  list()
  temp <- get_hl7_line(data, term)
  temp <-  str_split(temp, fixed("|"), simplify = T)
  if(!is.na(temp[1])){if(!nchar(temp[1])==0)
  {result$FELD <-temp[1]}}
  if(!is.na(temp[2])){if(!nchar(temp[2])==0)
  {result$TRENN_ZEICHEN <- temp[2]}}
  if(!is.na(temp[3])){if(!nchar(temp[3])==0)
  {result$SENDING_APPLICATION <- temp[3]}}
  if(!is.na(temp[4])){if(!nchar(temp[4])==0)
  {result$SENDING_FACILITY <- temp[4]}}
  if(!is.na(temp[5])){if(!nchar(temp[5])==0)
  {result$RECEIVER_APPLICATION <- temp[5]}}
  if(!is.na(temp[6])){if(!nchar(temp[6])==0)
  {result$RECEIVER_FACILITY <- temp[6]}}
  if(!is.na(temp[7])){if(!nchar(temp[7])==0){
    DATE_TIME <- get_date_time(temp[7])
    result$DATE <- DATE_TIME$DATE
    result$TIME <- DATE_TIME$TIME
  }}
  if(!is.na(temp[8])){if(!nchar(temp[8])==0)
  {result$SECURITY <- temp[8]}}
  if(!is.na(temp[9])){if(!nchar(temp[9])==0) {
    MESSAGE_TYPE <- temp[9]
    MESSAGE_TYPE <- str_split(MESSAGE_TYPE, fixed("^"), simplify = T)
    if(!is.na(MESSAGE_TYPE[1])){if(!nchar(MESSAGE_TYPE[1])==0)
    {result$MESSAGE_TYPE <- MESSAGE_TYPE[1]}}
    if(!is.na(MESSAGE_TYPE[2])){if(!nchar(MESSAGE_TYPE[2])==0)
    {result$EREIGNIS_CODE <- MESSAGE_TYPE[2]}}
  }}
  if(!is.na(temp[10])){if(!nchar(temp[10])==0)
  {result$MESSAGE_CONTROL_ID <- temp[10]}}
  if(!is.na(temp[11])){if(!nchar(temp[11])==0)
  {result$PROCESSING_ID <- temp[11]}}
  if(!is.na(temp[12])){if(!nchar(temp[12])==0)
  {result$VERSION_ID <- temp[12]}}
  if(!is.na(temp[13])){if(!nchar(temp[13])==0)
  {result$SEQUENCE_NUMBER <- temp[13]}}
  if(!is.na(temp[14])){if(!nchar(temp[14])==0)
  {result$CONTINUATION_POINTER <- temp[14]}}
  if(!is.na(temp[15])){if(!nchar(temp[15])==0)
  {result$ACCEPT_ACKNOWLEDGEMENT_TYPE <- temp[15]}}
  if(!is.na(temp[16])){if(!nchar(temp[16])==0)
  {result$APPLICATION_ACKNOWLEDGEMENT_TYPE <- temp[16]}}
  if(!is.na(temp[17])){if(!nchar(temp[17])==0)
  {result$COUNTRY_CODE <- temp[17]}}
  if(!is.na(temp[18])){if(!nchar(temp[18])==0)
  {result$CHARACTER_SET <- temp[18]}}
  if(!is.na(temp[19])){if(!nchar(temp[19])==0)
  {result$PRINCIPAL_LANGUAGE <- temp[19]}}
  if(!is.na(temp[20])){if(!nchar(temp[20])==0)
  {result$ALTERNATE_CHARACTER_SET <- temp[20]}}
  if(!is.na(temp[21])){if(!nchar(temp[21])==0)
  {result$MESSAGE_PROFILE_ID <- temp[21]}}
  if(!is.na(temp[22])){if(!nchar(temp[22])==0)
  {result$SENDING_RESPONSIBLE_ORGANISATION <- temp[22]}}
  if(!is.na(temp[23])){if(!nchar(temp[23])==0)
  {result$RECEIVING_RESPONSIBLE_ORGANISATION <- temp[23]}}
  if(!is.na(temp[24])){if(!nchar(temp[24])==0)
  {result$SENDING_NETWORK_ADDRESS <- temp[24]}}
  if(!is.na(temp[25])){if(!nchar(temp[25])==0)
  {result$RECEIVING_NETWORK_ADDRESS <- temp[25]}}
  # Length of the result shall not be ZERO otherwise NULL
  if(!length(result) == 0){
    return(result)
  }else{
    return(NULL)
  }
}
