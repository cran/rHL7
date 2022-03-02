#'
#' @title Prepares EVENT with random information as a list
#' @return returns a list of list
#' @description generates a list of lists, with evn that cotains the information
#' from EVN and the line as its should be written in the .hl7 file.
#' @importFrom stringr str_split
write_evn <- function(){
  res <- list()
  evn <- list()
  evn$FELD <- 'EVN'
  evn$EVENT_ID <- 'A01'
  evn$DATE <- format(Sys.time(), '%Y%m%d')
  evn$TIME <- format(Sys.time(), '%H%M%S')
  res$evn <- evn
  res$line <- paste(evn$FELD, evn$EVENT_ID, paste0(evn$DATE,evn$TIME), sep = "|")
  return(res)
}
