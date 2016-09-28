#' Compute daily curves from data
#'
#' Split the daily data by date and compute a curve for each day using a chosen \code{model} ('NS'/'NSS' for Nelson Siegel
#' and Svensson)
#' @param bond_list a list of bond objects
#' @param daily_data a dataframe contains daily data of the bonds constructs to be compatible with \code{\link{curve_model}}
#' and can be extracted from Bloomberg using \code{\link{get_daily_data}}.
#' @param min_obs an integer. The minimal number of bond's price observation in order to compute a curve for a certain date.
#' @param model A string indicating the model to use - 'NS' for Nelson Siegel and 'NSS' for Svensson.
#' @param adj_dur A logical indicates weather the calculation should be duration adjusted.
#' @param adj_vol A logical indicates weather the calculation should be volume adjusted.
#' @param max_vol A numeric indicates a maximal trade volume to be considered when calculating the volume adjustment for each bond.
#' @return a list containing:
#' \enumerate{
#'  \item model name (string)
#'  \item is duration adjusted (logical)
#'  \item is volume adjusted (logical)
#'  \item maximum volume for consideration (numeric)
#'  \item dataframe of the results
#'  }
#' @importFrom dplyr filter select
#' @export
make_params_for_all_dates <- function(bond_list, daily_data, min_obs = 6, model = "NS", adj_dur = TRUE, adj_vol = FALSE, 
    max_vol = NULL) {
    # define initial guess for the model and define a blank dataframe for the results
    if (model == "NS") {
        init_guess <- c(4, -4, -3, 3)
        result <- data.frame(date = as.Date("2000-01-01", format = "%d-%m-%Y"), b0 = 0, b1 = 0, b2 = 0, lambda = 0, 
            min_term = 0, max_term = 0, num_obs = 0)
    } else if (model == "NSS") {
        init_guess <- c(4, -4, -3, 0.1, 3, 6.8)
        result <- data.frame(date = as.Date("2000-01-01", format = "%d-%m-%Y"), b0 = 0, b1 = 0, b2 = 0, b3 = 0, 
            lambda1 = 0, lambda2 = 0, min_term = 0, max_term = 0, num_obs = 0)
    }
    result <- result[-1, ]
    num_model <- length(init_guess)
    
    # split the data by date
    data_by_date <- split(daily_data, daily_data$date)
    
    # define a counter (because for dates with few observations there will be no calculation)
    count <- 1
    # a loop that computes curve for each date and then takes the last computation as the initial guess for
    # the next date
    for (i in seq_along(data_by_date)) {
        data <- data_by_date[[i]]
        # take out bonds with maturity less than 9 months
        data <- filter(data, term > (9/12))
        # if data has more observations than min_obs then calculate:
        if (nrow(data) > min_obs) {
            calc_date <- data$date[1]
            market_data <- select(data, market_price:name)
            result[count, "date"] <- calc_date
            
            # last calculation as initial guess
            model_result <- curve_model(bond_list, market_data, calc_date, model = model, init_guess = init_guess, 
                adj_dur = adj_dur, adj_vol = adj_vol, max_vol = max_vol)
            result[count, 2:(2 + num_model - 1)] <- model_result$pars
            result[count, (2 + num_model):(2 + num_model + 1)] <- range(data$term)
            result[count, (2 + num_model + 2)] <- nrow(data)
            # take the calculation as the next initial guess
            init_guess <- model_result$pars
            count <- count + 1
            print(calc_date)
        }
    }
    return(list(model = model, adj_dur = adj_dur, adj_vol = adj_vol, max_vol = max_vol, result = result))
}


