#' Get daily data for a list of bonds
#'
#' A function that gets a Bloomberg SRCH, list of bonds and a start date and returns a data frame
#' with data for each date on each bond (market prices,trade volumes,time to maturity)
#' @param srch_name A string. A bloomberg saved SRCH name.
#' @param bond_list A list of bond objects.
#' @param start_date A date to start getting the data from.
#' @param end_date A date. Last date to get data from.
#' @return A dataframe with the following columns:
#' \itemize{
#'    \item date
#'    \item market price
#'    \item trade volume
#'    \item name of the bond
#'    \item maturity date of the bond
#'    \item time to maturity of the bond in the \code{date}
#' }
#' @seealso \code{\link{create_all_bonds}} for a function that creates a bond list that can be used in \code{get_daily_data}
#' from a Bloomberg SRCH.
#' @importFrom dplyr mutate arrange select %>%
#' @export
get_daily_data <- function(srch_name, bond_list, start_date, end_date=NULL) {
    # Connect to bloomberg
    con <- Rblpapi::blpConnect()
    # get tickers list from a bloomberg custom SRCH
    x <- as.character(Rblpapi::bsrch(paste0("FI:", srch_name))$id)
    x <- gsub(" Corp", "@TASE Corp", x)
    # get bonds main data
    bond_names <- x %>% Rblpapi::bdp(c("ID_BB", "SERIES", "MATURITY")) %>% mutate(ID_BB = paste0(substr(ID_BB, 1, nchar(ID_BB) -
        1), "@TASE Corp"))
    bond_names <- bond_names %>%
                  mutate(SERIES = ifelse(SERIES=="",paste0(format(MATURITY,"%m%y")),SERIES)) %>%
                  select(ID_BB, SERIES)
    # get bonds daily data
    daily_data <- x %>% Rblpapi::bdh(c("PX_LAST", "VOLUME"), start.date = start_date, end.date=end_date)
    # Disconnect bloomberg
    Rblpapi::blpDisconnect(con)
    # change the names of the lists to the series name
    names(daily_data) <- bond_names[match(names(daily_data), bond_names$ID_BB), 2]
    # Add the name of the series to a new column
    daily_data <- lapply(seq_along(daily_data), function(i) mutate(daily_data[[i]], name = names(daily_data)[i]))
    # change NA to 0 in the volume
    daily_data <- lapply(daily_data, function(x) replace(x, is.na(x), 0))
    # Unlist the data
    daily_data <- do.call(rbind, daily_data)
    # Add maturity and arrange by date and maturiy
    maturs <- apply(daily_data, 1, function(x) bond_by_name(bond_list, x[4])$maturity)
    daily_data <- daily_data %>% mutate(maturity = as.Date(maturs, origin = "1970-01-01")) %>% arrange(date,
        maturity) %>% mutate(term = as.numeric((maturity - date)/365)) %>% select(date, market_price = PX_LAST,
        trade_volume = VOLUME, name:term)
    return(daily_data)
}
