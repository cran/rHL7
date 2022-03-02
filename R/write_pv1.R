#'
#' @title Prepares PV1 with random information as a list
#' @return returns a list of list
#' @description generates a list of lists, with pv1 that cotains the information
#' from PV1 and the line as its should be written in the .hl7 file.
write_pv1 <- function(){

  ORGFA <- c("CHI","MED","ORT","AN2","ME2","OP",
             "CH2DEP", "GC2DEP", "OT2","CKGF","CKPL","OTR")
  ORGPF <- c("1A","1B", "2A", "3A", "3B", "AN2IB","ME2A","ME2AGRA", "ME2AGRC", "ME2B",
             "GEMZOP","GE2IOP", "CH2A", "CH2POI", "GC2A", "GC2POI", "CKGFST", "GEMIB",
             "ME2IB", "CHI1", "MED1" )


  orgfa <- sample(unique(ORGFA), 1)
  orgpf <- sample(unique(ORGPF), 1)

  res <- list()
  pv1 <- list()
  # 1
  pv1$FELD <- 'PV1'
  # 2
  SET_ID_PATIENT_VISIT <- ''
  # 3
  pv1$PATIENT_CLASS <- sample(c('I','O'), 1)
  # 4
  pv1$POINT_OF_CARE <- orgpf
  ROOM <- ''
  BED <- ''
  pv1$FACILITY <- orgfa
  LOCATION_STATUS <- ''
  PERSON_LOCATION_TYPE <- ''
  BUILDING <- ''
  FLOOR <- ''
  LOCATION_TYPE <- ''
  PATIENT_LOCATION <- paste(pv1$POINT_OF_CARE,
                            ROOM,
                            BED,
                            pv1$FACILITY,
                            LOCATION_STATUS,
                            PERSON_LOCATION_TYPE,
                            BUILDING,
                            FLOOR,
                            LOCATION_TYPE,
                            sep = '^')
  # 5
  pv1$ADMISSION_TYPE <- as.character(sample(c(5, 6, 7, 8, 9, 10, 11, 17, 18, 19, 20, 21, 22), 1))
  # 6
  PRE_ADMIT_NUMBER <- ''
  # 7
  PRIOR_PATIENT_LOCATION <- ''
  # 8
  ATTENDING_DOCTOR <- ''
  # 9
  REFERRING_DOCTOR <- ''
  # 10
  pv1$CONSULTING_DOCTOR <- as.character(sample(seq(100000,999999), 1))
  # 11
  HOSPITAL_SERVICE <- ''
  # 12
  TEMPORARY_LOCATION <- ''
  # 13
  PREADMIT_TEST_INDICATOR <- ''
  # 14
  pv1$READMISSION_INDICATOR <- as.character(sample(c('AN','AO','AT','AW', 'BK', 'BN','NG', 'NK', 'TK',
                                                     'EN', 'EO', 'ER', 'ET', 'EV', 'BK', 'BN', 'WA',
                                                     'WK','WS','ZK', 'ZN', 'AB', 'EH', 'FE', 'GU',
                                                     'KO','NB', 'OP', 'PA', 'PS', 'SP', 'TE', 'UE', 'ZL',
                                                     'ZW', 'BA', 'EA'), 1)) # excluded WZ and WB
  # 15
  ADMIT_SOURCE <- ''
  # 16
  AMBULATORY_STATUS <- ''
  # 17
  VIP_IDICATOR <- ''
  # 18
  ADMITTING_DOCTOR <- ''
  # 19
  PATIENT_TYPE <- ''
  # 20
  pv1$VISIT_NUMBER <- as.character(sample(seq(1000000001, 9999999999), 1)) # FALNR
  # 21
  FINANCIAL_CLASS <- ''

  var22 <- ''; var23 <- ''; var24 <- ''; var25 <- ''; var26 <- ''; var27 <- '';
  var28 <- ''; var29 <- ''; var30 <- ''; var31 <- ''; var32 <- ''; var33 <- '';
  var34 <- ''; var35 <- ''; var36 <- ''; var37 <- ''; var38 <- ''; var39 <- '';
  var40 <- ''; var41 <- ''; var42 <- ''; var43 <- ''; var44 <- '';
  var47 <- ''; var48 <- ''; var49 <- ''; var50 <- ''

  # 45
  pv1$ADMITTED_DATE <- as.character(format(Sys.time(), '%Y%m%d%H%M%S'))
  # 46
  DISCHARGE_DATE <- ''
  # 51
  pv1$ALTERNATIVE_VISIT_ID <- str_pad(as.character(sample(seq(0:99),1)), width = 5, pad = '0',side = 'left')

  line <- paste(pv1$FELD, SET_ID_PATIENT_VISIT,
                pv1$PATIENT_CLASS,
                PATIENT_LOCATION,
                pv1$ADMISSION_TYPE,
                PRE_ADMIT_NUMBER,
                PRIOR_PATIENT_LOCATION,
                ATTENDING_DOCTOR,
                REFERRING_DOCTOR,
                pv1$CONSULTING_DOCTOR,
                HOSPITAL_SERVICE,
                TEMPORARY_LOCATION,
                PREADMIT_TEST_INDICATOR,
                pv1$READMISSION_INDICATOR,
                ADMIT_SOURCE,
                AMBULATORY_STATUS,
                VIP_IDICATOR,
                ADMITTING_DOCTOR,
                PATIENT_TYPE,
                pv1$VISIT_NUMBER,
                FINANCIAL_CLASS,
                var22, var23, var24, var25, var26, var27, var28, var29, var30, var31,
                var32, var33, var34, var35, var36, var37, var38, var39, var40, var41,
                var42, var43, var44,
                pv1$ADMITTED_DATE,
                DISCHARGE_DATE,
                var47, var48, var49, var50,
                pv1$ALTERNATIVE_VISIT_ID, sep= '|')
  res$pv1 <- pv1
  res$line <- line
  return(res)
}
