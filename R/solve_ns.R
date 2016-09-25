# Functions to create a Nelson Siegel interpolation zero curve
# using a bonds list, market prices and optionaly trade volumes.

# the function accepts a list of bonds (bond class), a data frame of market data and a calculation date.
# the market data should have a "name" column that has names from the bonds list,
# a "market_price" column and optionally a "trade_volume" column.

curve_ns <- function(bonds_list,market_data,calc_date,init_guess=c(4,-4,-3,3),adj_dur=TRUE,adj_vol=FALSE,max_vol=NULL) {
  #calculate paramteres 4 times:
  # 1. with the initial guess by user
  # 2. with the default initial guess
  # 3. with 2 random initial guesses (uniform)
  # and take the best solution

  init_guesses <- matrix(0,nrow=4,ncol=4)
  init_guesses[1,] <- init_guess
  init_guesses[2,] <- c(4,-4,-3,3)
  init_guesses[3,] <- runif(4,min=c(0,-15,-30,0),max=c(15,30,30,10))
  init_guesses[4,] <- runif(4,min=c(2,-7,-5,2),max=c(6,7,5,5))

  #if adj_dur==TRUE adds duration column to market_data
  if (adj_dur==TRUE) {
    market_data <- cbind(market_data,duration=
                          apply(market_data,1,function(x) calc_dur_name(bonds_list,x["name"],calc_date,as.numeric(x["market_price"]))))
  }

  #Create a data frame that contains all bonds from the list and their market_data
  #vector of the names
  bonds_list_names <- vapply(bonds_list,function(x) x$name,character(1))
  #Get the bond names that appear in both lists
  act_names <- intersect(bonds_list_names,market_data$name)
  #subset the bonds_list to only the bonds from market_data
  bonds_list <- bonds_list[vapply(bonds_list,function(x) x$name %in% act_names,logical(1))]

  cost_func <- function(init_guess) {
    #get the model prices of the bond using the current guess
    bonds_model_prices <- vapply(bonds_list,function(x) price_bond_model(x,calc_date,model="NS",model_params=init_guess),numeric(1))
    model_bonds <- data.frame(name=act_names,model_price=bonds_model_prices)
    # merge both
    bonds_data <- merge(model_bonds,market_data)

    # if adj_vol==TRUE calculates total volume for simplicity in the calculations
    if (adj_vol==TRUE) {
      total_vol <- sum(bonds_data$trade_volume)
    }
    # calculate cost function
    if(adj_dur==FALSE && adj_vol==FALSE) {
      #without adjustments only take sum of squares of differences
      cost <- sum((bonds_data$model_price-bonds_data$market_price)^2)
      # Adjust to duration
    } else if (adj_dur==TRUE && adj_vol==FALSE) {
      cost <- sum(((bonds_data$model_price-bonds_data$market_price)/(bonds_data$duration))^2)
      # Adjust to volume
    } else if (adj_dur==FALSE && adj_vol==TRUE) {
      cost <- sum(((bonds_data$model_price-bonds_data$market_price)*(bonds_data$trade_volume/total_vol))^2)
      # Adjust both duration and volume
    } else if (adj_dur==TRUE && adj_vol==TRUE) {
      cost <- sum(((bonds_data$model_price-bonds_data$market_price)*(bonds_data$trade_volume/total_vol)/(bonds_data$duration))^2)
    }
    return(cost)
  }
  sols <- list()
  for (i in 1:4) {
    sols[[i]] <- Rsolnp::solnp(init_guesses[i,],cost_func,LB=c(0,-15,-30,0),UB=c(15,30,30,10))
  }
  return(sols[[which.min(vapply(sols,function(x) min(x$values),numeric(1)))]])
}


