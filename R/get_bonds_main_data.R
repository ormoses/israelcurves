#' Get SRCH data from bloomberg
#'
#' Code to get the data from the bloomberg SRCH using bsrch function (a custom SRCH needed to be saved)
#' The code gets the main attributes of each bonnd found in the search as well as each bond's cashflow.
#' @param srch_name A string. A bloomberg saved SRCH name.
#' @return a list containing 2 items:
#' \enumerate{
#'    \item A dataframe contains the bonds main data
#'    \item A list where each item is a bond's cashflow (normalized to 100)
#'  }
#' @importFrom dplyr mutate arrange filter %>%
#' @export
get_bond_data <- function(srch_name) {
    # Connect to bloomberg
    con <- Rblpapi::blpConnect()

    # get tickers list from a bloomberg custom SRCH
    x <- as.character(Rblpapi::bsrch(paste0("FI:", srch_name))$id)
    # get bonds main data
    bond_data <- x %>% Rblpapi::bdp(c("SECURITY_NAME", "ISSUE_DT", "CPN", "MATURITY", "FIRST_CPN_DT", "SERIES", "BASE_CPI",
        "ID_BB")) %>% mutate(ID_BB = paste0(substr(ID_BB, 1, nchar(ID_BB) - 1), " Corp")) %>% arrange(MATURITY)
    bond_data <- mutate(bond_data ,SERIES = ifelse(SERIES=="",paste0(format(MATURITY,"%m%y")),SERIES))
    # get bonds' cashflows
    ovrd <- c(USER_LOCAL_TRADE_DATE = "19900101")
    cashflows <- mapply(Rblpapi::bds, x, MoreArgs = list("DES_CASH_FLOW", overrides = ovrd))

    # disconnect bloomberg
    Rblpapi::blpDisconnect(con)

    # filter only the not null values
    cashflows <- cashflows[!unlist(lapply(cashflows, is.null))]
    # add the series as the name, payment date as date, and normalize the cashflow to 100, and add issue date
    cashflows <- lapply(seq_along(cashflows), function(x, nms, issue_dts, base_cpi, i) {
        list(name = nms[[i]], dates = x[[i]]$`Payment Date`, payments = (x[[i]]$`Coupon Amount` + x[[i]]$`Principal Amount`) *
            100/max(x[[i]]$`Principal Amount`), issue_date = issue_dts[[i]], known_CPI = base_cpi[[i]])
    }, x = cashflows, nms = vapply(seq_along(cashflows), function(i, nms) {
        bond_data[bond_data$ID_BB == nms[[i]], "SERIES"]
    }, nms = names(cashflows), character(1)),
    issue_dts = as.Date(vapply(seq_along(cashflows), function(i,nms) {
        bond_data[bond_data$ID_BB == nms[[i]], "ISSUE_DT"]
    }, nms = names(cashflows), numeric(1)), origin = "1970-01-01"),
    base_cpi = vapply(seq_along(cashflows), function(i, nms) {
      bond_data[bond_data$ID_BB == nms[[i]], "BASE_CPI"]
    }, nms = names(cashflows), numeric(1))
    )
    # change the name to the series name
    return(list(bond_data = bond_data, cashflows = cashflows))
}


#' Convert the bloomberg data into bond class
#
#' A function that takes a list of bonds' cashflow and an item number and creates a bond object
#' from the matching item in the list.
#' @param bond_cf a list. A list of bond cashflows created by \code{\link{get_bond_data}}.
#' @param n A number indicates the item from the list to create bond from.
#' @return a bond object
#' @seealso \code{\link{get_bond_data}} for getting the data from Bloomberg.
#' @export

create_bond_from_data <- function(bond_cf, n) {
    x <- bond_cf[[n]]
    bond(dates = x$dates, payments = x$payments, name = x$name, issue_date = x$issue_date, known_CPI = x$known_CPI)
}

#' Create a list of bond objects from the bloomberg data
#'
#' A function that takes a list of bonds' cashflows and creates a list of bond objects
#' @inheritParams create_bond_from_data
#' @return a list of bond objects
#' @seealso \code{\link{get_bond_data}} for getting the data from Bloomberg,
#'  \code{\link{create_bond_from_data}} for creating one bond.
#' @export
create_bonds <- function(bond_cf) {
    bonds <- Map(create_bond_from_data, seq_along(bond_cf), MoreArgs = list(bond_cf = bond_cf))
    return(bonds)
}

#' Create a list of bond objects using Bloomberg SRCH
#'
#' A function that takes a Bloomberg save SRCH and creates a list of bond objects from this search.
#' @inheritParams get_bond_data
#' @return a list of bond objects
#' @seealso \code{\link{get_bond_data}} for getting the data from Bloomberg,
#'  \code{\link{create_bond_from_data}} for creating one bond and \code{\link{create_bonds}} for creating a list of bonds.
#' @export
#'
create_all_bonds <- function(srch_name) {
    create_bonds(get_bond_data(srch_name)$cashflows)
}

