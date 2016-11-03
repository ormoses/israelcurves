#' Calculate main bond attributes
#'
#' A function that calculates 3 main attributes for a bond:
#' \itemize{
#'  \item Yield to Maturity (ytm)
#'  \item Duration And modified duration
#'  \item Convexity
#' }
#' @param thebond A bond object.
#' @param calc_date A date. The calculation date.
#' @param market_price A number. The price of the bond to calculate by.
#' @param ex_day (optional) A number indicating the Ex-day in the month where the bond pays coupon.
#' @param cpi_list list of the known CPI by date
#' @inheritParams create_vanilla_bond
#' @return A list with 4 items: yield to maturity, duration, modified duration and convexity
#' @importFrom stats uniroot
#' @seealso \code{\link{calc_bond_name}}
#' @export
calc_bond <- function(thebond, calc_date, market_price, cpi_list, ex_day = NULL, year_days = 365) {

    # uses positive_CF to get the positive cash flow as of the discount date
    pos_CF <- positive_CF(thebond, calc_date, cpi_list, ex_day, year_days)

    # The function of the CashFlow to find the root (the yield)
    bond_CF <- function(y) {
        sum(pos_CF$payments/((1 + y)^(pos_CF$pos_terms))) - market_price
    }
    # calculate YTM
    ytm <- (uniroot(bond_CF, c(-0.03, 0.15), tol = 1e-06)$root)
    # calculate duration
    dur <- sum((pos_CF$pos_terms * pos_CF$payments)/((1 + ytm)^(pos_CF$pos_terms)))/market_price
    # calculate modified duration
    mod_dur <- dur/(1 + ytm)
    # calculate convexity
    conv <- (sum(((pos_CF$pos_terms + pos_CF$pos_terms^2) * pos_CF$payments)/((1 + ytm)^(pos_CF$pos_terms)))/market_price)/(1 +
        ytm)^2
    return(list(ytm = ytm, duration = dur, mod_duration = mod_dur, covexity = conv))
}

#' Calculate bond attributes by name
#'
#' A function that calculates the bond attributes from \code{\link{calc_bond}} by bonds' name.
#' The function gets a list of bonds, name of bond, date and market price and calculates the attributes.
#' @param bonds_list a list containing bond objects
#' @param bond_name a string. The name of the bond.
#' @inheritParams calc_bond
#' @return A list with 4 items: yield to maturity, duration, modified duration and convexity
#' @seealso \code{\link{calc_bond}}
#' @export
calc_bond_name <- function(bonds_list, bond_name, calc_date, market_price, cpi_list, ex_day = NULL, year_days = 365) {
    thebond <- bond_by_name(bonds_list, bond_name)
    calc_bond(thebond, calc_date, market_price, cpi_list, ex_day, year_days)
}



