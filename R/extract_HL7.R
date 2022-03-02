#'
#' @title Extracts information from hl7 file
#' @param file is the path of the file
#' @return returns a data.frame with column names MANDT, PATID, PATNR, FALNR, LFDNR, BEWTY, BWART, TITLE,
#' VORNAME,NACHNAME, GEBDAT, GSCHL, FAMST, KLASSE, ORGPF, ORGFA, DATUM, UHRZEIT.
#' @description extracts the data from hl7 file and return a data.frame with a set of defined column names.
#' @importFrom dplyr bind_rows

extract_HL7 <-  function(file){
  # read content from the hl7 as descirbed in read_hl7_file function
  data <- read_hl7_file(file)
  # returns the file name with extension, simply attaches the file name and
  # extention extracted from fileparts function from matlab package

  # if the content of the hl7 file is null, the do the following and return NULL
  if(is.null(data)){
    # Move HL7 file to error folder for the inspection
    message('hl7 has no content.')
    return(NULL)
  }
  # if there exists content then
  # get the required information from hl7 read hl7
  MANDT <- ifelse(is.null(data$msh$SENDING_FACILITY),
                  {
                    NA
                  },
                  data$msh$SENDING_FACILITY)

  PATID <- ifelse(is.null(data$pid$EXTERNAL_PATIENT_ID)|
                    nchar(data$pid$EXTERNAL_PATIENT_ID)!=10,
                  {
                    NA
                  },
                  data$pid$EXTERNAL_PATIENT_ID)

  PATNR <- ifelse(is.null(data$pid$INTERNAL_PATIENT_ID)|
                    nchar(data$pid$INTERNAL_PATIENT_ID)!=10,
                  {
                    NA
                  },
                  data$pid$INTERNAL_PATIENT_ID)
  FALNR <- ifelse(is.null(data$pv1$VISIT_NUMBER),
                  {
                    NA
                  },
                  data$pv1$VISIT_NUMBER)
  LFDNR <- ifelse(is.null(data$pv1$ALTERNATIVE_VISIT_ID),
                  {
                    NA
                  },
                  data$pv1$ALTERNATIVE_VISIT_ID)
  VORNAME <- ifelse(is.null(data$pid$FIRST_NAME),
                    {
                      ""
                    },
                    data$pid$FIRST_NAME)
  NACHNAME <- ifelse(is.null(data$pid$LAST_NAME),
                     {
                       ""
                     },
                     data$pid$LAST_NAME)
  TITLE <- ifelse(is.null(data$pid$TITEL),
                  {
                    ""
                  },
                  data$pid$TITEL)
  GEBDAT <- ifelse(is.null(data$pid$DOB),
                   {
                     ""
                   },
                   data$pid$DOB)
  KLASSE <- ifelse(is.null(data$pv1$PATIENT_TYPE),
                   {
                     ""
                   },
                   data$pv1$PATIENT_TYPE)
  GSCHL <- ifelse(is.null(data$pid$GENDER),
                  {
                    ""
                  },
                  data$pid$GENDER)
  FAMST <- ifelse(is.null(data$pid$MARITAL_STATUS),
                  {
                    ""
                  },
                  data$pid$MARITAL_STATUS)

  BEWTY <- ifelse(is.null(data$pv1$ADMISSION_TYPE),
                  {
                    ""
                  },
                  data$pv1$ADMISSION_TYPE)

  BWART <- ifelse(is.null(data$pv1$READMISSION_INDICATOR),
                  {
                    ""
                  },
                  data$pv1$READMISSION_INDICATOR)

  ORGPF <- ifelse(is.null(data$pv1$POINT_OF_CARE),
                  {
                    ""
                  },
                  data$pv1$POINT_OF_CARE)

  ORGFA <- ifelse(is.null(data$pv1$FACILITY),
                  {
                    ""
                  },
                  data$pv1$FACILITY)
  DATUM <- ifelse(is.null(data$msh$DATE),
                  {
                    Sys.Date()
                  },
                  data$msh$DATE)
  UHRZEIT <- ifelse(is.null(data$msh$TIME),
                    {
                      format(Sys.time(), "%H%M%S")
                    },
                    data$msh$TIME)

  # combining the information into a row
  result <- cbind.data.frame(MANDT, PATID, PATNR, FALNR, LFDNR, BEWTY, BWART, TITLE,
                             VORNAME,NACHNAME, GEBDAT, GSCHL, FAMST, KLASSE,
                             ORGPF, ORGFA, DATUM, UHRZEIT,
                             stringsAsFactors = F)



  if(nrow(result)>0){
    msg <- paste("Data Extraction from HL7", fileparts(file)$name, 'SUCESS!')
    message(msg)
    return(result)
  }else{
    message('hl7 has zero rows')
  }

}
