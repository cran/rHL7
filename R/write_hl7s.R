require(stringr)
require(stringi)
require(randomNames)
require(dplyr)
require(matlab)

#' @title writes hl7s with random data.
#' @param write_to_file is a binary input TRUE/FALSE with TRUE as default, FALSE prints to screen
#' @param nr_files is an integer greater than 0, if missing, function writes single file.
#' @param return is a binary input TRUE/FALSE. TRUE would deliver an input a list
#' @return returns a data.frame with hl7 data with a next line  as a seperator
#' @description write_hl7s function writes hl7 files with random data that generates
#' for A09 transactions. This function writes the headers MSH, EVENT, PID, PV1, PV2

write_hl7s <- function(write_to_file = TRUE, nr_files, return = TRUE){

  if(missing(write_to_file)){
    write_to_file <-  TRUE
    }

  if(missing(nr_files)){
    nr_flies <- 1
    }

  if(missing(return)){
    return_df <- FALSE
  }else{
    return_df <- TRUE
  }


  res <- list()
  lis <- list()
  text_df <- data.frame(stringsAsFactors = F)
  msh_df <- data.frame(stringsAsFactors = F)
  event_df <- data.frame(stringsAsFactors = F)
  pid_df <- data.frame(stringsAsFactors = F)
  pv1_df <- data.frame(stringsAsFactors = F)
  pv2_df <- data.frame(stringsAsFactors = F)
  temp_hl7_data <- data.frame(stringsAsFactors = F)

  for(i in 1:nr_files){
    msh <- write_msh()
    evn <- write_evn()
    pid <- write_pid()
    pv1 <- write_pv1()
    pv2 <- write_pv2()

    txt <- unlist(
      stri_split_lines(
        paste(msh$line,
              evn$line,
              pid$line,
              pv1$line,
              pv2$line,
              sep = '\n'),
        omit_empty = TRUE))



    if(isTRUE(write_to_file)){
      path <- getwd()
      path <- file.path(path,'hl7s')
      if(!dir.exists(path)){
        dir.create(path)
      }
      file_name <- file.path(path,
                             paste0(sample(c("Pat_", "Pat_"), 1), pid$pid$EXTERNAL_PATIENT_ID,
                                    format(Sys.time(), "%Y%m%d%H%M%S"),'.hl7'))
      write.table(txt, file_name,
                  col.names = FALSE,
                  row.names = FALSE,
                  quote = FALSE, eol = "\r")
    }

    text_df <- bind_rows(text_df, data.frame(HL7_TEXT = paste(txt, collapse = '\n'), stringsAsFactors = F))
  }

  if(isTRUE(return)){
    # message(text_df)
    return(text_df)
  }
}
