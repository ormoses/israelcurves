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
  act <- as.Date(paste0(year(a),"-",ifelse(month(a)==1,12,month(a)-1),"-","01"))
  data.frame(known=known,act=act)
}
