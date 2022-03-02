
#' @title extract content from a hl7
#' @description This function will extract content of the hl7. However, the content will not have the lable of the elements
#' @param file the path of the hl7 file
#' @return a list with headers and their corresponding elements

extract_content_from_hl7 <- function(file){

  res <- list()

  lines <- readLines(file)
  length(lines)
  for(line in lines){
    l <- unlist(str_split(line, '\\|'))
    header <- l[1]
    for(element in 2:length(l)){
      res[[paste0(header)]][[element-1]] <- l[element]
    }
  }

  return(res)
}

