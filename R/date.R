#' filter by date range
#'
#' @param dt data.table or vector
#' @param date_range date ranges: c(date_start, date_end)
#' @param DateName the name of the Date column in data table
#'
#' @return
#' @export
#'
#' @examples
filter_date <- function(dt, date_range, DateName = 'Date'){
  if(is.data.table(dt)) dt %<>% setDT()
  if(is.list(dt)){
    dt[[Date]] %<>% as_date
    date_range %<>% as_date
    # setnames(dt, Date, 'Date')
    return(dt[dt[[Date]] >= date_range[1] & dt[[Date]] <= date_range[2]])
  }
  else{
    tmp <- function(x, date_range){
      x <- x[x >= date_range[1] & x <= date_range[2]]
    }
    return(tmp(dt, date_range))
  }
}

#' add explicit date info
#'
#' @param dt data.table
#' @param DateName date ranges: c(date_start, date_end)
#' @param extra_indices extra options, c('yday', 'dn'). yday is the DOY (days of year)
#' @param n day order of dn, 7th is weekly and 8th is used for MODIS
#'
#' @return  Year, Month, Day and extra indices
#' @export
#'
#' @examples
add_date_info <- function(dt, DateName = 'Date', extra_indices = NULL,  n = 8){
  if(is.data.table(dt)) dt %<>% setDT()
  dt %<>% .[, `:=`(Year = year(dt[[DateName]]),
                   Month = month(dt[[DateName]]),
                   Day = day(dt[[DateName]])
  )]
  if(sum(str_detect(extra_indices, 'yday'))){
    dt %<>% .[, yday := yday(dt[[DateName]])]
  }
  if(sum(str_detect(extra_indices, 'dn'))){
    add_dn <- function(date, n){
      DOY <- yday(date)
      dn <- as.integer(ceiling((DOY)/n))
      return(dn)
    }
    dt %<>% .[, dn := add_dn(dt[[DateName]], n)]
  }
}
