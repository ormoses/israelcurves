# Add CPI functionality
# function to add:
# 1. get_cpi_from_bloomberg
# 2. get_known_cpi(date,base=1998)

#' Get CPI release dates from bloomberg
#'
#' A function that gets the release and actual CPI dates from bloomberg (From 2001)
#' @return a dataframe with release dates and actual dates
get_release_dates_from_bloomberg <- function() {
  con <- blpConnect()
  dates <- bdh("ISCPIMMN Index","ECO_RELEASE_DT",start.date=as.Date("1990-01-01"))
  blpDisconnect(con)
  known <- as.Date(dates$ECO_RELEASE_DT)
  act <- as.Date(dates$date)
  data.frame(known=known,act=act)
}

#' Get CPI Index from bloomberg
#'
#' A function to get the CPI Index from bloomberg using the last base
get_cpi_from_bloomberg <- function() {
  con <- blpConnect()
  cpi <- bdh("ISCPINM Index","PX_LAST",start.date=as.Date("2001-01-01"))
  blpDisconnect(con)
  names(cpi) <- c("act","index")
  return(cpi)
}

#merge the two functions "get_release_dates_from_bloomberg" and "get_cpi_from_bloomberg"
get_cpi_data <- function() {
  release <- get_release_dates_from_bloomberg()
  cpi <- get_cpi_from_bloomberg()
  merge(release,cpi)
}
