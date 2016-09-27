#Split the data by name and get the NS

make_params_for_all_dates <- function(bond_list,daily_data,min_obs=6,model="NS",adj_dur=TRUE,adj_vol=FALSE) {
  #define initial guess for the NS
  init_guess=c(4,-4,-3,3)

  #split the data by date
  data_by_date <- split(daily_data,daily_data$date)
  #Define a blank dataframe for the results
  result <- data.frame(date=as.Date("2000-01-01",format="%d-%m-%Y"),b0=0,b1=0,b2=0,lambda=0,min_term=0,max_term=0,num_obs=0)
  result <- result[-1,]
  #define a counter (because for dates with few observations there will be no calculation)
  count <- 1
  #a loop that computes NS for each date and then takes the last computation as the initial guess
  #for the next date
  for (i in seq_along(data_by_date)) {
    data <- data_by_date[[i]]
    #take out bonds with maturity less than 9 months
    data <- dplyr::filter(data,term>(9/12))
    #if data has more observations than min_obs then calculate:
    if (nrow(data)>min_obs) {
      calc_date <- data$date[1]
      market_data <- dplyr::select(data,market_price:name)
      result[count,"date"] <- calc_date

      # last calculation as initial guess
      NS <- curve_model(bond_list,market_data,calc_date,init_guess,adj_dur=adj_dur,adj_vol=adj_vol)
      result[count,2:5] <- NS$pars
      result[count,6:7] <- range(data$term)
      result[count,8] <- nrow(data)
      #take the calculation as the next initial guess
      init_guess <- NS$pars
      count <- count+1
      print(calc_date)
    }
  }
  return(list(model=model,adj_dur=adj_dur,adj_vol=adj_vol,result=result))
}


