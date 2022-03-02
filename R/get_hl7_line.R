#' @title get the details of the hl7 content based on the header
#' @param data hl7 content
#' @param term the header
# This function gets every line that has a particular term in the line
# test protocol included

get_hl7_line <- function(data, term){ ####
  if(missing(data)){
    stop('data is missing in get_hl7_line')
  }
  if(missing(term)){
    stop("term is missing in get_hl7_line")
  }
  return(data[grepl(term, data, # ignore.case = T,
                    fixed = TRUE)])
}
