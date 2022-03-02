
#'
#' @title Prepares PV2 with random information as a list
#' @return returns a list of list
#' @description generates a list of lists, with pv2 that cotains the information
#' from PV2 and the line as its should be written in the .hl7 file.

write_pv2 <- function(){
  pv2 <- list()
  res <- list()
  pv2$FELD <-  'PV2'
  pv2$PRIOR_PENDING_LOCATION <- 'a'
  pv2$ACCOMMODATION_TYPE <- 'b'

  pv2$ID <- 'c'
  pv2$TEXT <- 'd'
  ADMISSION_REASON <- paste(pv2$ID, pv2$TEXT, sep = '^')
  res$pv2 <- pv2
  res$line <- paste(pv2$FELD, pv2$PRIOR_PENDING_LOCATION,
                    pv2$ACCOMMODATION_TYPE, ADMISSION_REASON, sep = '|')
  return(res)
}
