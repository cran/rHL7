#' @title Reads PID line from hl7
#' @description This function gets the data from PID line of HL7 file
#' @aliases Sai Pavan Kumar Veeranki
#' @param data text from hl7 file
#' @return a list with the elements from PID header
#' @importFrom stringr str_split
#' @importFrom stringr fixed
get_PID <- function(data){
  if(missing(data)){
    stop("data is missing for .get_PID")
  }
  term <- 'PID'
  result <- list()
  temp <- get_hl7_line(data, term)
  temp <-  str_split(temp, fixed('|'),simplify = T)

  if(!is.na(temp[1])){if(!nchar(temp[1])==0)
  {result$FELD <- temp[1]}}

  if(!is.na(temp[2])){if(!nchar(temp[2])==0)
  {result$SET_ID <- temp[2]}}

  if(!is.na(temp[3])){if(!nchar(temp[3])==0)
  {result$EXTERNAL_PATIENT_ID <- temp[3]}} # PATID

  if(!is.na(temp[4])){if(!nchar(temp[4])==0)
  {result$INTERNAL_PATIENT_ID <- temp[4]}} # PATNR

  if(!is.na(temp[5])){if(!nchar(temp[5])==0)
  {result$ALTERNATIVE_PATIENT_ID <- temp[5]}} #

  if(!is.na(temp[6])){if(!nchar(temp[6])==0){
    PATIENT_NAME <- str_split(temp[6],fixed("^"),simplify = T)
    if(!is.na(PATIENT_NAME[1])){if(!nchar(PATIENT_NAME[1])==0)
    {result$FIRST_NAME <- PATIENT_NAME[1]}}
    if(!is.na(PATIENT_NAME[2])){if(!nchar(PATIENT_NAME[2])==0)
    {result$LAST_NAME <- PATIENT_NAME[2]}}
    if(!is.na(PATIENT_NAME[3])){if(!nchar(PATIENT_NAME[3])==0)
    {result$SECOND_NAME <- PATIENT_NAME[3]}}
    if(!is.na(PATIENT_NAME[4])){if(!nchar(PATIENT_NAME[4])==0)
    {result$SUFFIX <- PATIENT_NAME[4]}}
    if(!is.na(PATIENT_NAME[5])){if(!nchar(PATIENT_NAME[5])==0)
    {result$PREFIX <- PATIENT_NAME[5]}}
    if(!is.na(PATIENT_NAME[6])){if(!nchar(PATIENT_NAME[6])==0)
    {result$TITEL <- PATIENT_NAME[6]}}
  }}

  if(!is.na(temp[7])){if(!nchar(temp[7])==0)
  {result$BIRTH_NAME <- temp[7]}}

  if(!is.na(temp[8])){if(!nchar(temp[8])==0)
  {result$DOB <- temp[8]}}

  if(!is.na(temp[9])){if(!nchar(temp[9])==0)
  {result$GENDER <- temp[9]}}

  if(!is.na(temp[10])){if(!nchar(temp[10])==0)
  {result$ALIAS <- temp[10]}}

  if(!is.na(temp[11])){if(!nchar(temp[11])==0)
  {result$ETHNICITY <- temp[11]}}

  if(!is.na(temp[12])){if(!nchar(temp[12])==0){
    ADDRESS <- str_split(temp[12], fixed('^'), simplify = T)
    if(!is.na(ADDRESS[1])){if(!nchar(ADDRESS[1])==0)
    {result$STREET1 <- ADDRESS[1]}}
    if(!is.na(ADDRESS[2])){if(!nchar(ADDRESS[2])==0)
    {result$STREET2 <- ADDRESS[2]}}
    if(!is.na(ADDRESS[3])){if(!nchar(ADDRESS[3])==0)
    {result$CITY <- ADDRESS[3]}}
    if(!is.na(ADDRESS[4])){if(!nchar(ADDRESS[4])==0)
    {result$PROVINCE <- ADDRESS[4]}}
    if(!is.na(ADDRESS[5])){if(!nchar(ADDRESS[5])==0)
    {result$POSTAL_CODE <- ADDRESS[5]}}
    if(!is.na(ADDRESS[6])){if(!nchar(ADDRESS[6])==0)
    {result$COUNTRY <- ADDRESS[6]}}
  }}

  if(!is.na(temp[13])){if(!nchar(temp[13])==0)
  {result$COUNTRY_CODE <- temp[13]}}

  if(!is.na(temp[14])){if(!nchar(temp[14])==0)
  {result$TELEPHONE_HOME <- temp[14]}}

  if(!is.na(temp[15])){if(!nchar(temp[15])==0)
  {result$TELEPHONE_WORK <- temp[15]}}

  if(!is.na(temp[16])){if(!nchar(temp[16])==0)
  {result$MOTHER_TONGUE <- temp[16]}}

  if(!is.na(temp[17])){if(!nchar(temp[17])==0)
  {result$MARITAL_STATUS <- temp[17]}}

  if(!is.na(temp[18])){if(!nchar(temp[18])==0)
  {result$RELIGION <- temp[18]}}

  if(!is.na(temp[19])){if(!nchar(temp[19])==0)
  {result$PATIENT_ADMISSION_NUMBER <- temp[19]}}

  if(!is.na(temp[20])){if(!nchar(temp[20])==0)
  {result$SOCIAL_SECURITY_NUMBER <- temp[20]}}

  if(!is.na(temp[21])){if(!nchar(temp[21])==0)
  {result$DRIVERS_LICENSE_NUMBER <- temp[21]}}

  if(!is.na(temp[22])){if(!nchar(temp[22])==0)
  {result$MOTHERS_IDENTIFIER <- temp[22]}}

  if(!is.na(temp[23])){if(!nchar(temp[23])==0)
  {result$ETHNIC_GROUP <- temp[23]}}

  if(!is.na(temp[24])){if(!nchar(temp[24])==0)
  {result$BIRTH_PLACE <- temp[24]}}

  if(!is.na(temp[25])){if(!nchar(temp[25])==0)
  {result$MULTIPLE_BIRTH_ORDER <- temp[25]}}

  if(!is.na(temp[26])){if(!nchar(temp[26])==0)
  {result$BIRTH_ORDER <- temp[26]}}

  if(!is.na(temp[27])){if(!nchar(temp[27])==0)
  {result$CITIZENSHIP <- temp[27]}}

  if(!is.na(temp[28])){if(!nchar(temp[28])==0)
  {result$VETERANS_MILITARY_STATUS <- temp[28]}}

  if(!is.na(temp[29])){if(!nchar(temp[29])==0)
  {result$NATIONALITY_CODE <- temp[29]}}

  if(!is.na(temp[30])){if(!nchar(temp[30])==0){
    PATEINT_DEATH_DATE_TIME <- get_date_time(temp[30])
    result$PATIENT_DEATH_DATE <- PATEINT_DEATH_DATE_TIME$DATE
    result$PATIENT_DEATH_TIME <- PATEINT_DEATH_DATE_TIME$TIME
  }}

  if(!is.na(temp[31])){if(!nchar(temp[31])==0)
  {result$PATIENT_DEATH_IDENTIFIER <- temp[31]}}

  if(!is.na(temp[32])){if(!nchar(temp[32])==0)
  {result$IDENTITY_UNKOWN_INDICATOR <- temp[32]}}

  if(!is.na(temp[33])){if(!nchar(temp[33])==0)
  {result$IDENTITIY_RELIABILITY_CODE <- temp[33]}}

  if(!is.na(temp[34])){if(!nchar(temp[34])==0){
    LAST_UPDATE_DATE_TIME <- get_date_time(temp[34])
    result$LAST_UPDATE_DATE <- PATEINT_DEATH_DATE_TIME$DATE
    result$LAST_UPDATE_TIME <- PATEINT_DEATH_DATE_TIME$TIME
  }}

  if(!is.na(temp[35])){if(!nchar(temp[35])==0)
  {result$LAST_UPDATE_FACILITY <- temp[35]}}

  if(!is.na(temp[36])){if(!nchar(temp[36])==0)
  {result$SPECIES_CODE <- temp[36]}}

  if(!is.na(temp[37])){if(!nchar(temp[37])==0)
  {result$SPECIES_CODE <- temp[37]}}

  if(!is.na(temp[38])){if(!nchar(temp[38])==0)
  {result$BREED_CODE <- temp[38]}}

  if(!is.na(temp[39])){if(!nchar(temp[39])==0)
  {result$STRAIN <- temp[39]}}

  if(!is.na(temp[40])){if(!nchar(temp[40])==0)
  {result$TRIBAL_CITIZENSHIP <- temp[40]}}

  if(!length(result) == 0){
    return(result)
  }else{
    return(NULL)
  }
}
