#' @title reading PV1 header content
#' @description reads the content of the header PV1
#' @param data hl7 content
#' @importFrom stringr str_split
#' @importFrom stringr fixed
get_PV1 <- function(data){ ####
  if(missing(data)){
    stop('data is missing in .get_PV1')
  }
  term <- 'PV1'
  result <- list()
  temp <- get_hl7_line(data, term)
  temp <- str_split(temp, fixed('|'), simplify = T)
  if(!is.na(temp[1])){if(!nchar(temp[1])==0)
  {result$FELD <- temp[1]}}
  if(!is.na(temp[2])){if(!nchar(temp[2])==0)
  {result$SET_ID_PATIENT_VISIT <- temp[2]}}
  if(!is.na(temp[3])){if(!nchar(temp[3])==0)
  {result$PATIENT_CLASS <- temp[3]}}
  if(!is.na(temp[4])){if(!nchar(temp[4])==0){
    PATIENT_LOCATION <- str_split(temp[4], fixed('^'), simplify = T)
    if(!is.na(PATIENT_LOCATION[1])){if(!nchar(PATIENT_LOCATION[1])==0)
    {result$POINT_OF_CARE <- PATIENT_LOCATION[1]}}
    if(!is.na(PATIENT_LOCATION[2])){if(!nchar(PATIENT_LOCATION[2])==0)
    {result$ROOM <- PATIENT_LOCATION[2]}}
    if(!is.na(PATIENT_LOCATION[3])){if(!nchar(PATIENT_LOCATION[3])==0)
    {result$BED <- PATIENT_LOCATION[3]}}
    if(!is.na(PATIENT_LOCATION[4])){if(!nchar(PATIENT_LOCATION[4])==0)
    {result$FACILITY<- PATIENT_LOCATION[4]}}
    if(!is.na(PATIENT_LOCATION[5])){if(!nchar(PATIENT_LOCATION[5])==0)
    {result$LOCATION_STATUS <- PATIENT_LOCATION[5]}}
    if(!is.na(PATIENT_LOCATION[6])){if(!nchar(PATIENT_LOCATION[6])==0)
    {result$PERSON_LOCATION_TYPE <- PATIENT_LOCATION[6]}}
    if(!is.na(PATIENT_LOCATION[7])){if(!nchar(PATIENT_LOCATION[7])==0)
    {result$BUILDING <- PATIENT_LOCATION[7]}}
    if(!is.na(PATIENT_LOCATION[8])){if(!nchar(PATIENT_LOCATION[8])==0)
    {result$FLOOR <- PATIENT_LOCATION[8]}}
    if(!is.na(PATIENT_LOCATION[9])){if(!nchar(PATIENT_LOCATION[9])==0)
    {result$LOCATION_TYPE <- PATIENT_LOCATION[9]}}
  }}
  if(!is.na(temp[5])){if(!nchar(temp[5])==0)
  {result$ADMISSION_TYPE <- temp[5]}}
  if(!is.na(temp[6])){if(!nchar(temp[6])==0)
  {result$PRE_ADMIT_NUMBER <- temp[6]}}
  if(!is.na(temp[7])){if(!nchar(temp[7])==0)
  {result$PRIOR_PATIENT_LOCATION <- temp[7]}}
  if(!is.na(temp[8])){if(!nchar(temp[8])==0)
  {result$ATTENDING_DOCTOR <- temp[8]}}
  if(!is.na(temp[9])){if(!nchar(temp[9])==0)
  {result$REFERRING_DOCTOR <- temp[9]}}
  if(!is.na(temp[10])){if(!nchar(temp[10])==0)
  {result$CONSULTING_DOCTOR <- temp[10]}}
  if(!is.na(temp[11])){if(!nchar(temp[11])==0)
  {result$HOSPITAL_SERVICE <- temp[11]}}
  if(!is.na(temp[12])){if(!nchar(temp[12])==0)
  {result$TEMPORARY_LOCATION <- temp[12]}}
  if(!is.na(temp[13])){if(!nchar(temp[13])==0)
  {result$PREADMIT_TEST_INDICATOR <- temp[13]}}
  if(!is.na(temp[14])){if(!nchar(temp[14])==0)
  {result$READMISSION_INDICATOR <- temp[14]}}
  if(!is.na(temp[15])){if(!nchar(temp[15])==0)
  {result$ADMIT_SOURCE <- temp[15]}}
  if(!is.na(temp[16])){if(!nchar(temp[16])==0)
  {result$AMBULATORY_STATUS <- temp[16]}}
  if(!is.na(temp[17])){if(!nchar(temp[17])==0)
  {result$VIP_IDICATOR <- temp[17]}}
  if(!is.na(temp[18])){if(!nchar(temp[18])==0)
  {result$ADMITTING_DOCTOR <- temp[18]}}
  if(!is.na(temp[19])){if(!nchar(temp[19])==0)
  {result$PATIENT_TYPE <- temp[19]}}
  if(!is.na(temp[20])){if(!nchar(temp[20])==0)
  {result$VISIT_NUMBER <- temp[20]}}
  if(!is.na(temp[21])){if(!nchar(temp[21])==0)
  {result$FINANCIAL_CLASS <- temp[21]}}
  #' Charge price indicator to prior temporary lcoation are not being read
  if(!is.na(temp[45])){if(!nchar(temp[45])==0)
  {result$ADMITTED_DATE <- temp[45]}}
  if(!is.na(temp[46])){if(!nchar(temp[46])==0)
  {result$DISCHARGE_DATE <- temp[46]}}
  #' Current patient balance to total payments are not being read.
  if(!is.na(temp[51])){if(!nchar(temp[51])==0)
  {result$ALTERNATIVE_VISIT_ID <- temp[51]}}
  if(!is.na(temp[52])){if(!nchar(temp[52])==0)
  {result$VISIT_INDICATOR <- temp[52]}}
  if(!is.na(temp[53])){if(!nchar(temp[53])==0)
  {result$OTHER_HEALTHCARE_PROVIDER <- temp[53]}}
  if(!length(result) == 0){
    return(result)
  }else{
    return(NULL)
  }
}
