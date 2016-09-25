# Functions to get the bonds of a curve for a period specified by the user
# Using the bloomberg API - using BDS function that takes curve ticker and curve date
# the main function will return a character vector with the tickers

#A function that gets a curve ticker (without the "INDEX" part) and a certain date
# and returns a dataframe with one column that contains the tickers of all members of
# the curve in the same date

get_members_for_date <- function(curve_ticker,curve_date) {
  #Check connection to bloomberg, if isn't connected - connect
  require(Rblpapi)
  t_value <- try(suppressWarnings(defaultConnection()))
  if ("try-error" %in% class(t_value))    con <- blpConnect()
  #Get the members from bloomberg using BDS
  members <- bds(paste0(curve_ticker," INDEX"),"CURVE MEMBERS",overrides=c("CURVE_DATE"=curve_date))
  names(members) <- curve_date
  return(members)
}


#A function that gets a curve ticker (without the "INDEX" part) and a period of dates
# and returns a character vector the tickers of all members of the curve in those dates
# the period variable should be "day" or "month" or "year"
get_members_for_period <- function(curve_ticker,start_date,end_date=start_date,period="month") {
  #Check connection to bloomberg, if isn't connected - connect
  require(Rblpapi)
  t_value <- try(suppressWarnings(defaultConnection()))
  if ("try-error" %in% class(t_value))    con <- blpConnect()
  #check that end_date>start_date
  if (end_date<start_date) stop("end_date must be after start_date")
  #Get the members list using get_members_for_date
  members_list <- mapply(get_members_for_date,curve_ticker,seq(start_date,end_date,by=paste0("1 ",period)))
  members_list <- unique(unlist(members_list))
  return(members_list)
}

