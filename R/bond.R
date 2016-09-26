# Function to define class - bond
# The class is a list of payment_dates (date), payments (numeric) that should be on the same length
bond <- function(dates,payments,face_value=100,name=NULL,issue_date=NULL,type=NULL,known_CPI=NULL) {
  # Check that dates and payments are at the same length
  if (length(dates)!=length(payments)) {
    stop("dates and payments should be on the same length")
  }
  lastdate <- format(tail(dates,1),"%m%y")
  if (is.null(name)) {
    name <- lastdate
  }
  setClass("bond",representation =
             list(name="character",dates="Date",payments="numeric",issue_date="Date",maturity="Date",
                  face_value="numeric",type="character",known_CPI="numeric"))
  bond <- list(name=name,dates=dates,payments=payments,issue_date=issue_date,maturity=tail(dates,1),
               face_value=face_value,type=type,known_CPI=known_CPI)
  class(bond) <- "bond"
  return(bond)
}

# Function that creates a vanilla bond
# Payment frequency is the number of payments per year
# Coupon is in percentage, term is in years
create_vanilla_bond <- function(issue_date,first_payment,term,coupon,name=NULL,eom=TRUE,
                                payment_frequency=1,face_value=100,year_days=365,type=NULL,known_CPI=NULL) {
  #Create the payment dates
  if (eom==TRUE) {
    dates <- c(issue_date,seq(
      lubridate::update(first_payment,year=year(first_payment),month=month(first_payment)+1,mday=1),
      by=paste0(12/payment_frequency," month"),length=term+1)-1)
  } else {
    dates <- c(issue_date,seq(first_payment,by=paste0(12/payment_frequency," month"),length=term+1))
  }

  #Calculate how many days each coupon has accumulates
  coupon_days <- diff(dates)
  #remove the issue date from the payment dates because there is no payment there
  dates <- dates[-1]
  #calculate each coupon
  coupons <- as.numeric(coupon_days/year_days*coupon*face_value)
  #add the face value to the last payment (vanilla bond)
  coupons[length(coupons)] <- coupons[length(coupons)]+face_value
  #return as class bond
  return(bond(dates=dates,payments=coupons,name=name,issue_date=issue_date,type=type,known_CPI=known_CPI))
}

#Summary method for bond class
summary.bond <- function(bond) {
  cat("\n","Bond Summary:","\n\n")
  cat("A ",trimws(as.character(round(as.numeric(diff(range(bond$dates))/365),0))),
      " years ",ifelse(is.null(bond$type),"",bond$type)," bond.\n")
  cat("Bond name: ",bond$name,"\n")
  cat("Issue Date: ",format(bond$issue_date,"%d/%m/%Y"),"\n")
  cat("Maturity: ",format(bond$maturity,"%d/%m/%Y"),"\n")
  cat("Face Value: ",bond$face_value,"\n")
  cat("\n","Cash Flow:\n")
  print(data.frame(dates=bond$dates,payments=bond$payments))
}

#print method for bond class
print.bond <- function(bond) {
  cat("Bond name: ",bond$name,"\n")
  cat("\n","Cash Flow:\n")
  print(data.frame(dates=bond$dates,payments=bond$payments))
}

#plot method for bond class
plot.bond <- function(bond) {
  nums <- length(bond$payments)
  interest <- bond$payments
  interest[nums] <- interest[nums]-bond$face_value
  principal <- numeric(nums)
  principal[nums] <- bond$face_value
  bond_df <- data.frame(dates=bond$dates,interest=interest,principal=principal)
  bond_df <- tidyr::gather(bond_df,key=type,value=total,-1)
  bond_df$type <- factor(bond_df$type)
  levels(bond_df$type) <- c("Interest","Principal")
  total_df <- data.frame(dates=bond$dates,total=bond$payments)
  ggplot2::ggplot()+geom_bar(aes(x=dates,y=total,fill=type),data=bond_df,stat="identity")+
                    geom_text(size=4,data=total_df,aes(x=dates,y=total+3,label=round(total,2)))+
                    theme_classic()+
                    theme(legend.position="right",legend.title=element_blank())+
                    scale_x_date(breaks=bond$dates)+
                    labs(x="Payment Date",y="Payment")+ggtitle(paste0("Bond Cash Flow for bond: ",bond$name))
}

# a function that gets a list of bonds and returns a bond type by its name
bond_by_name <- function(bond_list,name) {
  num <- match(name,vapply(bond_list,function(x) x$name,character(1)))
  bond <- bond_list[[num]]
  return(bond)
}



