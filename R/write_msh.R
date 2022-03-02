#'
#' @title Prepares MSH with random information as a list
#' @return returns a list of list
#' @description generates a list of lists, with msh that cotains the information
#' from MSH and the line as its should be written in the .hl7 file.

write_msh <- function(){
  res <- list()
  msh <- list()
  # 1
  msh$FELD <- 'MSH'
  # 2
  msh$TRENN_ZEICHEN <- "^~\\&"
  # 3
  msh$SENDING_APPLICATION <- 'SAP-ISH'
  loc <- sample(c("104", "107"), 1)
  # 4
  msh$SENDING_FACILITY <- loc
  # 5
  msh$RECEIVER_APPLICATION <- loc
  # 6
  msh$RECEIVER_FACILITY <- 'LEO'
  # 7
  msh$DATE <- format(Sys.time(), '%Y%m%d')
  msh$TIME <- format(Sys.time(), '%H%M%S')
  DATE_TIME <- paste0(msh$DATE, msh$TIME)
  # 8
  SECURITY <- ''
  # 9
  msh$MESSAGE_TYPE <- 'ADT'
  # 10
  msh$EREIGNIS_CODE <- 'A02'
  # 11
  MESSAGE_TYPE <- paste(msh$MESSAGE_TYPE, msh$EREIGNIS_CODE, sep = '^')
  # 12
  msh$MESSAGE_CONTROL_ID <- as.character(sample(seq(1000000001, 9999999999), 1))
  # 13
  msh$PROCESSING_ID <- 'P'
  # 14
  msh$VERSION_ID <-  '2.3'
  line <-  paste(msh$FELD,
                 msh$TRENN_ZEICHEN,
                 msh$SENDING_APPLICATION,
                 msh$SENDING_FACILITY,
                 msh$RECEIVER_APPLICATION,
                 msh$RECEIVER_FACILITY,
                 DATE_TIME,
                 SECURITY,
                 MESSAGE_TYPE,
                 msh$MESSAGE_CONTROL_ID,
                 msh$PROCESSING_ID,
                 msh$VERSION_ID,
                 sep = '|')
  res$msh <- msh
  res$line <- line
  return(res)
}
