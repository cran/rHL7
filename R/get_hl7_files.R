#' @title gets all hl7 files from a path
#' @description lists all the hl7 files in a location
#' @param files_location the path where the hl7 files exists
#'

get_hl7_files <- function(files_location){
  if(missing(files_location)){
    stop('Argument files_location is missing')
  }
  if(dir.exists(files_location)){
    files_location <- files_location
  }else{
    msg <- "files_location does not exist"
    stop(msg)
  }
  # TODO check only hl7 files
  hl7_files <- list.files(files_location, pattern = '.hl7', # ignore.case = TRUE,
                          full.names = T)
  Sys.chmod(hl7_files, mode = "0777", use_umask = F)

  return(hl7_files)


}

