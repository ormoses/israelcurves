# A functions for bonds calculations:
# 1. Calculate yield to maturity
# 2. Calculate Duration
# 3. Calculation Convexity


# A function that gets a bond, calculation date and the market price and 
# calculates the yield to maturity for the bond, the duration, modified duration and convexity
calc_bond <- function(thebond,calc_date,market_price,ex_day=NULL,year_days=365) {

  #uses positive_CF to get the positive cash flow as of the discount date
  pos_CF <- positive_CF(thebond,calc_date,ex_day,year_days)
  
  # The function of the CashFlow to find the root (the yield)
  bond_CF <- function(y) {
    sum(pos_CF$payments/((1+y)^(pos_CF$pos_terms)))-market_price
  }
  #calculate YTM
  ytm <- (uniroot(bond_CF,c(-0.03,0.15),tol=0.000001)$root)
  #calculate duration
  dur <- sum((pos_CF$pos_terms*pos_CF$payments)/((1+ytm)^(pos_CF$pos_terms)))/market_price
  #calculate modified duration
  mod_dur <- dur/(1+ytm)
  #calculate convexity
  conv <- (sum(((pos_CF$pos_terms+pos_CF$pos_terms^2)*pos_CF$payments)/((1+ytm)^(pos_CF$pos_terms)))/market_price)/(1+ytm)^2
  return(list(ytm=ytm,duration=dur,mod_duration=mod_dur,covexity=conv))
}


# a function that gets a list of bonds, name of bond, date and market price and calculates duration
calc_dur_name <- function(bonds_list,bond_name,calc_date,market_price,ex_day=NULL,year_days=365) {
  thebond <- bond_by_name(bonds_list,bond_name)
  calc_bond(thebond,calc_date,market_price,ex_day,year_days)$duration
}


  
