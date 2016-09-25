#Code to get the data from the bloomberg SRCH using bsrch function (a custom SRCH needed to be saved)
get_bond_data <- function(srch_name) {
  #Connect to bloomberg
  con <- Rblpapi::blpConnect()

  #get tickers list from a bloomberg custom SRCH
  x <- as.character(Rblpapi::bsrch(paste0("FI:",srch_name))$id)
  #get bonds main data
  bond_data <- x %>%
  Rblpapi::bdp(c("SECURITY_NAME","ISSUE_DT","CPN","MATURITY","FIRST_CPN_DT","SERIES","ID_BB")) %>%
  dplyr::mutate(ID_BB=paste0(substr(ID_BB, 1, nchar(ID_BB)-1)," Corp")) %>%
  dplyr::arrange(MATURITY)

  #get bonds' cashflows
  ovrd <- c("USER_LOCAL_TRADE_DATE"="19900101")
  cashflows <- mapply(Rblpapi::bds,x,MoreArgs = list("DES_CASH_FLOW",overrides=ovrd))

  #disconnect bloomberg
  Rblpapi::blpDisconnect(con)

  #filter only the not null values
  cashflows <- cashflows[!unlist(lapply(cashflows,is.null))]
  #add the series as the name, payment date as date, and normalize the cashflow to 100, and add issue date
  cashflows <- lapply(seq_along(cashflows),
                      function(x,nms,issue_dts,i) {list(name=nms[[i]],dates=x[[i]]$`Payment Date`,
                                        payments=(x[[i]]$`Coupon Amount`+x[[i]]$`Principal Amount`)*100/max(x[[i]]$`Principal Amount`),
                                        issue_date=issue_dts[[i]])},
                      x=cashflows,
                      nms=vapply(seq_along(cashflows),
                                 function(i,nms) {bond_data[bond_data$ID_BB==nms[[i]],"SERIES"]},nms=names(cashflows),character(1)),
                      issue_dts=as.Date(vapply(seq_along(cashflows),
                                               function(i,nms) {bond_data[bond_data$ID_BB==nms[[i]],"ISSUE_DT"]},nms=names(cashflows),
                                               numeric(1)),origin="1970-01-01"))
  #change the name to the series name
  return(list(bond_data=bond_data,cashflows=cashflows))
}


#Code to convert the bloomberg data into bond class
create_bond_from_data <- function(bond_data,n) {
  x <- bond_data[[n]]
 bond(dates=x$dates,payments=x$payments,name=x$name,issue_date=x$issue_date)
}

#Create a list of bonds from the bloomberg data
create_bonds <- function(bond_data) {
  bonds <- Map(create_bond_from_data,seq_along(bond_data),MoreArgs=list(bond_data=bond_data))
  return(bonds)
}

#A function to create bonds_list using the functions above
create_all_bonds <- function(srch_name) {
  create_bonds(get_bond_data(srch_name)$cashflows)
}

