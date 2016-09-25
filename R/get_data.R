require(tidyverse)
require(Rblpapi)


#A script to get the data
get_daily_data <- function(srch_name,bond_list) {
  #get tickers list from a bloomberg custom SRCH
  x <- as.character(bsrch(paste0("FI:",srch_name))$id)
  x <- gsub(" Corp","@TASE Corp",x)
  #get bonds main data
  bond_names <- x %>% 
                bdp(c("ID_BB","SERIES")) %>%
                mutate(ID_BB=paste0(substr(ID_BB, 1, nchar(ID_BB)-1),"@TASE Corp"))
  #get bonds daily data
  daily_data <- x %>%
    bdh(c("PX_LAST","VOLUME"),start.date=as.Date("2006-01-01"))
  #change the names of the lists to the series name
  names(daily_data) <- bond_names[match(names(daily_data),bond_names$ID_BB),2]
  #Add the name of the series to a new column
  daily_data <- lapply(seq_along(daily_data),function(i) mutate(daily_data[[i]],name=names(daily_data)[i]))
  #change NA to 0 in the volume
  daily_data <- lapply(daily_data,function(x) replace(x,is.na(x),0))
  #Unlist the data
  daily_data <- do.call(rbind,daily_data)
  #Add maturity and arrange by date and maturiy
  maturs <- apply(daily_data,1,function(x) bond_by_name(bond_list,x[4])$maturity)
  daily_data <- daily_data %>%
                mutate(maturity=as.Date(maturs,origin="1970-01-01")) %>%
                arrange(date,maturity) %>%
                mutate(term=as.numeric((maturity-date)/365)) %>%
                select(date,market_price=PX_LAST,trade_volume=VOLUME,name:term)
  return(daily_data)
}
