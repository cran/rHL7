#'
#' @title Prepares PID with random information as a list
#' @return returns a list of list
#' @description generates a list of lists, with pid that cotains the information
#' from PID and the line as its should be written in the .hl7 file.
write_pid <- function(){

  res <- list()
  pid <- list()

  # 1
  pid$FELD <- 'PID'
  # 2
  SET_ID <- ''
  # 3
  pid$EXTERNAL_PATIENT_ID <- as.character(sample(seq(1000000001, 9999999999), 1)) # PATID
  # 4
  pid$INTERNAL_PATIENT_ID <- as.character(sample(seq(1000000001, 9999999999), 1)) # PATNR
  # 5
  ALTERNATIVE_ID <- ''
  # 6 Name of the patient
  temp <- as.character(sample(c(0,1), replace=TRUE, size=1))
  switch(temp,
         '0' = {gender <- 'Male'; titel <- "Mag."; GSHL <-  'M'},
         '1' = {gender <- 'Female'; titel <- "Magin."; GSHL <- 'F'}
  )
  pid$FIRST_NAME <- randomNames( which.names = "first", ethnicity = "white", gender = gender)
  pid$LAST_NAME <- randomNames(which.names = "last", ethnicity = "white", gender = gender)
  second_name <- ''
  suffix <- ''
  prefix <- ''
  pid$TITEL <- titel
  name <- paste(pid$FIRST_NAME, pid$LAST_NAME, second_name, suffix, prefix, pid$TITEL, sep = '^')
  NAME <- name
  # 7 Birth name
  pid$BIRTH_NAME<- pid$FIRST_NAME
  # 8 DOB
  pid$DOB <- format(sample(seq(as.Date('1930/01/01'), as.Date('2000/12/31'), by="day"), 1), '%Y%m%d')
  # 9
  pid$GENDER <- GSHL
  # 10
  ALIAS <- ''
  # 11
  ETHNICITY <- ''
  # 12
  street1 <- ''
  street2 <- ''
  city <- ''
  province <- ''
  pid$POSTAL_CODE <- as.character(sample(seq(8000, 8900), 1))
  pid$COUNTRY <- 'Austria'
  ADDRESS <- paste(street1, street2, city, province, pid$POSTAL_CODE, pid$COUNTRY, sep = '^')
  COUNTRY_CODE <- ''
  TELEPHONE_PRV <- ''
  TELEPHONE_WORK <- ''
  MOTHER_TONGUE <- ''
  pid$MARITAL_STATUS <- as.character(sample(seq(0, 5), 1))
  RELIGION <- ''
  PATIENT_ADMISSION_NUMBER <- ''
  pid$SOCIAL_SECURITY_NUMBER <- as.character(sample(seq(1000000001, 9999999999), 1)) # SOCIAL_SECURITY_NUMBER
  line <-  paste(pid$FELD,
                 SET_ID,
                 pid$EXTERNAL_PATIENT_ID,
                 pid$INTERNAL_PATIENT_ID,
                 ALTERNATIVE_ID,
                 NAME,
                 pid$BIRTH_NAME,
                 pid$DOB,
                 pid$GENDER,
                 ALIAS,
                 ETHNICITY,
                 ADDRESS,
                 COUNTRY_CODE,
                 TELEPHONE_PRV,
                 TELEPHONE_WORK,
                 MOTHER_TONGUE,
                 pid$MARITAL_STATUS,
                 RELIGION,
                 PATIENT_ADMISSION_NUMBER,
                 pid$SOCIAL_SECURITY_NUMBER,
                 sep = '|')
  res$pid <- pid
  res$line <- line
  return(res)
}
