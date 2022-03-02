require(lubridate)
#' @title reads date and time
#' @description splits the string into date and time.
#' @param str expects an date and time as a string in the format %Y%m%d%H%M%S
#' datetime <- '2010-02-13 10:30:00'
#' get_date_time(datetime)
get_date_time <- function(str){
  if(missing(str)){
    stop('date time string is missing in get_data_time')
  }

  result <- list()

  if(nchar(str) <=1 ){
    str <- format(Sys.time(),'%Y%m%d%H%M%S')
  }

  if(nchar(str)>=14){
    str <- ymd_hms(str)
    str <- format(str, '%Y%m%d%H%M%S')
    result$DATE <- substring(str,1,8)
    result$TIME <- substring(str,9,14)
  }

  if(nchar(str)==8){
    result$DATE <- str
  }

  if(nchar(str)==6){
    result$TIME <- str
  }

  return(result)
}
