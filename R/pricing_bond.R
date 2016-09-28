#' Price a bond
#'
#'
#' A function that gets a bond, discount_date and a vector of rates to discount and returns the bond's price
#' as of the discount date
#' @param thebond A bond object
#' @param disc_date The discount date
#' @param rates A numeric vector of the discount rates corresponding to the payment dates
#' @inheritParams calc_bond
#' @return The price of the bond for the discount date (numeric)
#' @seealso \code{\link{price_bond_model}} to price a bond using a model calculated discount rates.
#' @export

price_bond <- function(thebond,disc_date,rates,ex_day=NULL,year_days=365) {

  #uses positive_CF to get the positive cash flow as of the discount date
  pos_CF <- positive_CF(thebond,disc_date,ex_day,year_days)

  #Check that the length of the rates and the cashflow is the same
  if (length(rates)!=length(pos_CF$pos_terms)) {
    stop(paste0("There are ",length(pos_CF$pos_terms)," dates and ",length(rates)," rates. Should be the same length."))
  }

  #Price the bond - sum of the discounted cashflow
  res <- sum(pos_CF$payments/((1+rates)^(pos_CF$pos_terms)))
  return(res)
}

#' Price a bond using rates from a model
#'
#' A function that prices a bond using a discount rates calculated from a model: Nelson Siegel or Svensson.
#' @param model A string indicating the model to use - "NS" for Nelson Siegel and "NSS" for Svensson.
#' @param model_params a numeric vector indicates the model parameters.
#' A 4-length for Nelson Siegel (NS) model and a 6-length for Svensson (NSS).
#' @inheritParams price_bond
#' @seealso \code{\link{price_bond}} to price a bond using a manual vector of discount rates.
#' @export
price_bond_model <- function(thebond,disc_date,model,model_params,ex_day=NULL,year_days=365) {

  #uses positive_CF to get the positive cash flow as of the discount date
  pos_CF <- positive_CF(thebond,disc_date,ex_day,year_days)
  #Calculate the rates
  rates <- calc_yields(pos_CF$pos_terms,model_params,model)

  #Price the bond - sum of the discounted cashflow
  res <- sum(pos_CF$payments/((1+rates)^(pos_CF$pos_terms)))
  return(res)
}

# Helper function that gets a bond and a date and returns a list
# With the positive terms and payments
# there is an option to add the day of the Ex-date and it will calculate it from this date
positive_CF <- function(thebond,thedate,ex_day=NULL,year_days=365) {

  #Computes the time to maturity from the discount date
  terms <- as.numeric((thebond$dates-thedate)/year_days)

  #Calculate the cashflow as of the discount_date
  payments <- thebond$payments[terms>0]
  pos_terms <- terms[terms>0]

  #check if ex_day is null, if not check if the date is between ex_date and payment date
  if (!is.null(ex_day)) {
    #find the date of the next payment
    next_payment <- thebond$dates[which(thebond$dates-thedate>0)[1]]
    #calculate next ex date
    next_ex <- as.Date(ISOdate(lubridate::year(next_payment),lubridate::month(next_payment),ex_day))
    #check if thedate is between the two dates
    is.ex <- thedate>=next_ex & thedate<next_payment
    #if is.ex is true then take out the first payment
    if (is.ex==TRUE) {
      pos_terms <- pos_terms[-1]
      payments <- payments[-1]
    }
  }

  return(list(pos_terms=pos_terms,payments=payments,terms=terms))
}
