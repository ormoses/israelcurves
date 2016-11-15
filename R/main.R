#' Wrapper function to calculate curves from data
#'
#' A wrapper function that gets the data from Bloomberg and computes the curves
#' @inheritParams get_daily_data
#' @inheritParams make_params_for_all_dates
#' @return a list containing:
#' \enumerate{
#'  \item model name (string)
#'  \item is duration adjusted (logical)
#'  \item is volume adjusted (logical)
#'  \item maximum volume for consideration (numeric)
#'  \item dataframe of the results
#' }
#' @export
build_curves <- function(srch_name,start_date,end_date=NULL,min_obs=6,
                         model="NS",adj_dur=TRUE,adj_vol=FALSE,max_vol=NULL,ex_day=NULL) {
  # Get the main bond data - bond_list
  bond_list <- create_all_bonds(srch_name)
  cat("===== Got all bonds ===== \n")
  # Get the daily data for the bonds
  daily_data <- get_daily_data(srch_name,bond_list,start_date,end_date = end_date)
  cat("===== Got the daily data ===== \n")
  # Get CPI list
  cpi_list <- get_cpi_data()
  cat("===== Got the CPI list ===== \n")
  # Compute the results
  result <- make_params_for_all_dates(bond_list, daily_data, cpi_list, min_obs, model, adj_dur, adj_vol, max_vol, ex_day)
  return(result)
}

#' A function that gets a curve for a vector of points and a certain date
#' @param maturities numeric. A vector of maturities to calculate yields.
#' @inheritParams build_curves
#' @return A vector of yields.
build_points_for_date <- function(maturities, srch_name, the_date, min_obs=6,
                         model="NS", adj_dur=TRUE, adj_vol=FALSE, max_vol=NULL, ex_day=NULL) {
  # Get the model parameters
  res <- build_curves(srch_name, the_date, the_date, min_obs, model, adj_dur, adj_vol, max_vol, ex_day=NULL)
  # Extract only the parameters
  if (model == "NS")  res <- as.numeric(res$result[1,2:5])
  else if (model == "NSS") res <- as.numeric(res$result[1,2:7])
  ylds <- calc_yields(maturities, res, model)
  names(ylds) <- maturities
  ylds
}
