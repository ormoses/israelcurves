#create a CPI base table
cpi_base_table <- function() {
  years <- c(1951, 1959, 1964, 1969, 1976, 1980, 1985, 1987, 1993, 1998, 2000, 2002, 2006, 2008, 2010, 2012, 2014)
  mult <- c(1, 2.753, 1.347, 1.237, 4.112, 8.349, 225, 1.775, 2.421, 1.581, 1.064, 1.068, 1.038, 1.051, 1.061, 1.052, 1.020)
  data.frame(year = years, mult = mult)
}

cpi_change_base <- function(cpi_index, old_year, new_year) {
  #get base table
  base_table <- cpi_base_table()
  #check base years validity
  if (!(old_year %in% base_table$year)) stop(paste0(old_year, " is not a base year"))
  if (!(new_year %in% base_table$year)) stop(paste0(new_year, " is not a base year"))
  if (new_year == old_year) stop("years has to be different")

  #find_places
  years_places <- match(c(old_year,new_year),base_table$year)
  #get connecting formula
  if (new_year < old_year) {
    mult <- prod(base_table$mult[(years_places[2]+1):(years_places[1])])
  } else if (new_year > old_year) {
    mult <- 1
  }
  mult * cpi_index

}


#' Get CPI release dates from bloomberg
#'
#' A function that gets the release and actual CPI dates from bloomberg (From 2001)
#' @importFrom Rblpapi blpConnect blpDisconnect bdh
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
#' @importFrom Rblpapi blpConnect blpDisconnect bdh
#' @return a dataframe with CPI index with the last base
get_cpi_from_bloomberg <- function() {
  con <- blpConnect()
  cpi <- bdh("ISCPINM Index","PX_LAST",start.date=as.Date("2001-01-01"))
  blpDisconnect(con)
  names(cpi) <- c("act","index")
  return(cpi)
}

#' Get CPI data from bloomberg
#'
#' Get the CPI data from bloomberg - the announcment (known) date, actual date and CPI index
#' using the last base
#'
#' @return a list consisting of the base year and dataframe with all CPI indices and dates.
#' @export
get_cpi_data <- function() {
  release <- get_release_dates_from_bloomberg()
  cpi <- get_cpi_from_bloomberg()
  list(data=merge(release,cpi),base=2014)
}

#' Get CPI index by date
#'
#' A function that gets a date and extracts the CPI index for this date.
#' @param calc_date date. The calculation date.
#' @param cpi_list list. A list of the cpi data from \code{\link{get_cpi_data}}
#' @return A number represents the CPI index on the calc_date.
#' @importFrom dplyr %>% filter select top_n
#' @export
cpi_by_date <- function(calc_date,cpi_list) {
  cpi_list$data %>%
    filter(calc_date - known > 0) %>%
    top_n(1,known) %>%
    select(index) %>%
    as.numeric
}
