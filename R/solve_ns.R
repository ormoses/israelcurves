#' Calculate a zero yield curve using a model for a certain day
#'
#' create a Nelson Siegel or Svensson interpolation zero curve using a bonds list, market prices and optionaly trade volumes
#' for a certain date. The optimization is done using Rsolnp package.
#' The basic cost function is:
#' \deqn{(P_{market}-P_{model})^{2}}
#' Duration adjusted equation is:
#' \deqn{\frac{(P_{market}-P_{model})^{2}}{Duration}}
#' Volume Adjusted equation is:
#' \deqn{{(P_{market}-P_{model})^{2}}\cdot{\frac{Volume}{Total Volume}}}
#' With both adjustment the equation is:
#' \deqn{\frac{(P_{market}-P_{model})^{2}}{Duration}\cdot{\frac{Volume}{Total Volume}}}
#' @param bonds_list a list of bond objects
#' @param market_data A dataframe. The known daily market data for the calculation date.
#' The dataframe should have a 'name' column that has names from the bonds list, a 'market_price' column
#' and optionally a 'trade_volume' column.
#' @param calc_date The calculation date.
#' @param init_guess the initial guess for the optimization algorithm.
#' @inheritParams make_params_for_all_dates
#' @return A vector of model parameters after optimization.
#' @export
curve_model <- function(bonds_list, market_data, calc_date, cpi_list, model="NS",
                        init_guess=NULL, adj_dur=TRUE,adj_vol=FALSE,max_vol=NULL,ex_day=NULL) {

  #how many parameters the model has and what are the bounds
  if (model=="NS") {
    num_model <- 4
    LB=c(0,-15,-30,0)
    UB=c(15,30,30,10)
    LB_narrow=c(2,-7,-5,2)
    UB_narrow=c(6,7,5,5)
    init_def <- c(4,-4,-3,3)
  } else if (model=="NSS") {
    num_model <- 6
    LB=c(0,-15,-30,-30,0,5)
    UB=c(15,30,30,30,10,10)
    LB_narrow=c(2,-7,-5,-5,2,7)
    UB_narrow=c(6,7,5,5,5,10)
    init_def <- c(4,-4,-3,0.1,3,6.8)
  }
  #if init_guess is NULL then add zeroes
  if (is.null(init_guess)) init_guess <- rep(0,num_model)

  #calculate paramteres 4 times:
  # 1. with the initial guess by user
  # 2. with the default initial guess
  # 3. with 2 random initial guesses (uniform)
  # and take the best solution

  init_guesses <- matrix(0,nrow=4,ncol=num_model)
  init_guesses[1,] <- init_guess
  init_guesses[2,] <- init_def
  init_guesses[3,] <- stats::runif(num_model,min=LB,max=UB)
  init_guesses[4,] <- stats::runif(num_model,min=LB_narrow,max=UB_narrow)
  #if adj_dur==TRUE adds duration column to market_data
  if (adj_dur==TRUE) {
    market_data <- cbind(market_data,duration=
                          apply(market_data,1,
                                function(x) calc_bond_name(bonds_list,x["name"],calc_date,as.numeric(x["market_price"]), cpi_list)$duration))
  }
  #if duration is NA remove the row
  market_data <- dplyr::filter(market_data,!is.na(duration))

  #Create a data frame that contains all bonds from the list and their market_data
  #vector of the names
  bonds_list_names <- vapply(bonds_list,function(x) x$name,character(1))
  #Get the bond names that appear in both lists
  act_names <- intersect(bonds_list_names,market_data$name)
  #subset the bonds_list to only the bonds from market_data
  bonds_list <- bonds_list[vapply(bonds_list,function(x) x$name %in% act_names,logical(1))]

  cost_func <- function(init_guess) {
    #get the model prices of the bond using the current guess
    bonds_model_prices <- vapply(bonds_list,function(x) price_bond_model(x,calc_date,model=model,model_params=init_guess, cpi_list = cpi_list, ex_day = ex_day),numeric(1))
    model_bonds <- data.frame(name=act_names,model_price=bonds_model_prices)
    # merge both
    bonds_data <- merge(model_bonds,market_data)

    # if adj_vol==TRUE calculates total volume for simplicity in the calculations
    if (adj_vol==TRUE) {
      if (!is.null(max_vol)) {
        if (max_vol<=0) stop("max_vol must be positive")
        bonds_data$trade_volume <- pmin(bonds_data$trade_volume,max_vol)
      }
      total_vol <- sum(bonds_data$trade_volume)
    }
        # calculate cost function
        if (adj_dur == FALSE && adj_vol == FALSE) {
            # without adjustments only take sum of squares of differences
            cost <- sum((bonds_data$model_price - bonds_data$market_price)^2)
            # Adjust to duration
        } else if (adj_dur == TRUE && adj_vol == FALSE) {
            cost <- sum(((bonds_data$model_price - bonds_data$market_price)/(bonds_data$duration))^2)
            # Adjust to volume
        } else if (adj_dur == FALSE && adj_vol == TRUE) {
            cost <- sum(((bonds_data$model_price - bonds_data$market_price) * (bonds_data$trade_volume/total_vol))^2)
            # Adjust both duration and volume
        } else if (adj_dur == TRUE && adj_vol == TRUE) {
            cost <- sum(((bonds_data$model_price - bonds_data$market_price) * (bonds_data$trade_volume/total_vol)/(bonds_data$duration))^2)
        }
        return(cost)
    }
    sols <- list()
    for (i in 1:4) {
        sols[[i]] <- Rsolnp::solnp(init_guesses[i, ], cost_func, LB = LB, UB = UB)
    }
    return(sols[[which.min(vapply(sols, function(x) min(x$values), numeric(1)))]])
}


