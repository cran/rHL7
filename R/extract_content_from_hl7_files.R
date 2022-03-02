#' @title extract content from multiple hl7 files
#' @description This function will extract content of the hl7. However, the content will not have the lable of the elements
#' @param files list of files
#' @param location the path of the hl7 files, in case of
#' @return a data.frame with headers and their corresponding elements with a sequence of column names, or NULL in case of no hl7 files


extract_content_from_hl7_files <- function(files, location){
  if(missing(files)){
    if(missing(location)){
      stop('both arguments files and location are missing')
    }else{
      files <- list.files(location, pattern = '.hl7', full.names = TRUE)
    }
  }

  if(missing(location)){
    if(missing(files)){
      stop('both arguments files and location are missing')
    }
  }else{
    if(missing(files)){
      files <- list.files(location, pattern = '.hl7', full.names = TRUE)
    }
  }

  res <- list()
  message(paste('Number of hl7 files:', length(files)))
  if(length(files)>0){
    for(file in files){
      result <- extract_content_from_hl7(file)
      if(is.list(result)){
        for(name in names(result)){
          # TODO get the respective column names based on the header name
          t <- as.data.frame(t(result[[name]]))
          # colnames(t) <- list from a repository.
          if(!is.null(res[[name]])){
            res[[name]] <- bind_rows(res[[name]],t)
          }else{
            res[[name]] <- t
          }
        }
      }
    }
    return(res)
  }else{
    return(NULL)
  }
}

