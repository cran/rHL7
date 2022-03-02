#' @title reads date hl7 file
#' @param file path of the hl7 file
#'
#' reads whole hl7 file and returns the content of it ####
read_hl7_file <- function(file){
  filename <- paste0(fileparts(file)$name, fileparts(file)$ext)

  if (missing(file)){
    msg <- "File is missng!"
    stop(msg)
  }

  if(!file.exists(file)){
    msg <- paste("File ", file, "does not exists!")
    message(msg)
    return(NULL)
  }

  msg <- paste('Reading', filename)
  message(msg)
  # get the content of the hl7
  result <- get_hl7_data(file = file)
  return(result)
}
