#is.error <- function(x) inherits(x, "try-error")

#daily_data2 <- dplyr::mutate(daily_data,ytm=0)
#for (i in seq_along(daily_data$date)) {
#  thebond <- bond_by_name(bond_list,daily_data$name[i])
#  ytm <- try(calc_bond(thebond,daily_data$date[i],daily_data$market_price[i],20)$ytm)
#  if (is.error(ytm)==TRUE) ytm <- NA
#  daily_data2$ytm[i] <- ytm*100
#}
