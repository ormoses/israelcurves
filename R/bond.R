#' Add a bond object (class bond)
#'
#' \code{bond} is a helper function for building a bond class object.
#'
#' @param dates A vector of payment dates
#' @param payments A vector of payments (has to be same size as dates)
#' @param face_value A number of the face value of the bond
#' @param name (optional) The name of the bond
#' @param issue_date (optional) The date the bond was issued
#' @param type (optional) A string represents the type of the bond
#' @param known_CPI (optional) A number represents the known CPI value at the time of issue of the bond
#' @return An object of class "bond"
#' @seealso \code{\link{create_vanilla_bond}}
#' @export
bond <- function(dates,payments,face_value=100,name=NULL,issue_date=NULL,type=NULL,known_CPI=NULL) {
  #As this is the main building block of the package we'll check the input thoroughly

    #Check that dates and payments are not NULL and not NA
    if ((sum(is.na(dates)) + sum(is.na(payments))) > 0) stop("dates and payments cannot have NA values")
    if ((is.null(dates)) | (is.null(payments))) stop("dates and payments have to have values")

    # Check that dates and payments are at the same length
    if (length(dates)!=length(payments)) {
      stop("dates and payments should be on the same length")
    }
    # Check that all payments are positive
    if (any(payments<=0)) stop("All payments must be positive")

    #If dates are not sorted - sort them and throw a warning
    if (!isTRUE(all.equal(dates,sort(dates)))) {
      warning("Dates input are not sorted, the function will sort them.")
      ord <- order(dates)
      dates <- dates[ord]
      payments <- payments[ord]
    }



  lastdate <- format(dates[length(dates)],"%m%y")
  if (is.null(name)) {
    name <- lastdate
  }
  thebond <- list(name=name,dates=dates,payments=payments,issue_date=issue_date,maturity=tail(dates,1),
               face_value=face_value,type=type,known_CPI=known_CPI)
  class(thebond) <- "bond"
  return(thebond)
}



#' Create vanilla bond object
#'
#' \code{create_vanilla_bond} is a simpler function than \code{\link{bond}}. It creates a bond object
#' using generic details instead of exact dates and payments. The bond is a vanilla one - with coupons
#' and one principal payment at maturity.
#' @param issue_date A date. The issue date of the bond.
#' @param first_payment A date. The date of the first coupon payment.
#' @param term A number. The term of the bond in years.
#' @param coupon A number. The coupon in percentage (for 5\% use 5)
#' @param eom logical. A logical variable that indicates if the payments are at the end of each month.
#' @param payment_frequency A number. The number of payments per year.
#' @param year_days A number. The number of days in each year.
#' @inheritParams bond
#' @return An object of class "bond"
#' @seealso \code{\link{bond}}
#' @export
create_vanilla_bond <- function(issue_date,first_payment,term,coupon,name=NULL,eom=TRUE,
                                payment_frequency=1,face_value=100,year_days=365,type=NULL,known_CPI=NULL) {
  #Create the payment dates
  if (eom==TRUE) {
    dates <- c(issue_date,seq(
      ymd(paste(year(first_payment),(month(first_payment)%% 12)+1,1,sep="-")),
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

#' Summary method for bond class
#' @param object a bond object (class "bond").
#' @param ... Additional parameters
#' @export
summary.bond <- function(object,...) {
  cat("\n","Bond Summary:","\n\n")
  cat("A ",trimws(as.character(round(as.numeric(diff(range(object$dates))/365),0))),
      " years ",ifelse(is.null(object$type),"",object$type)," bond.\n")
  cat("Bond name: ",object$name,"\n")
  cat("Issue Date: ",format(object$issue_date,"%d/%m/%Y"),"\n")
  cat("Maturity: ",format(object$maturity,"%d/%m/%Y"),"\n")
  cat("Face Value: ",object$face_value,"\n")
  cat("\n","Cash Flow:\n")
  print(data.frame(dates=object$dates,payments=object$payments))
}

#' Print method for bond class
#' @param x a bond object (class "bond").
#' @inheritParams summary.bond
#' @export
print.bond <- function(x,...) {
  cat("Bond name: ",x$name,"\n")
  cat("\n","Cash Flow:\n")
  print(data.frame(dates=x$dates,payments=x$payments))
}

#' Plot method for bond class
#'
#' Plot a cashflow of a bond.
#' @param x,y a bond object (class "bond").
#' @inheritParams summary.bond
#' @importFrom ggplot2 ggplot geom_bar geom_text theme_classic theme scale_x_date labs ggtitle
#' @export
plot.bond <- function(x,y=NULL,...) {
  nums <- length(x$payments)
  interest <- x$payments
  interest[nums] <- interest[nums]-x$face_value
  principal <- numeric(nums)
  principal[nums] <- x$face_value
  bond_df <- data.frame(dates=x$dates,interest=interest,principal=principal)
  bond_df <- tidyr::gather(bond_df,key=type,value=total,-1)
  bond_df$type <- factor(bond_df$type)
  levels(bond_df$type) <- c("Interest","Principal")
  total_df <- data.frame(dates=x$dates,total=x$payments)
  ggplot()+geom_bar(aes(x=dates,y=total,fill=type),data=bond_df,stat="identity")+
                    geom_text(size=4,data=total_df,aes(x=dates,y=total+3,label=round(total,2)))+
                    theme_classic()+
                    theme(legend.position="right",legend.title=element_blank())+
                    scale_x_date(breaks=x$dates)+
                    labs(x="Payment Date",y="Payment")+ggtitle(paste0("Bond Cash Flow for bond: ",x$name))
}

#' Extract bond by name
#'
#' A function that gets a list of bonds (bond class) and returns a bond object by its name.
#' @param bond_list a list containing bond objects
#' @param name A character contains the name of the bond.
#' @return A bond class object
#' @export
bond_by_name <- function(bond_list,name) {
  num <- match(name,vapply(bond_list,function(x) x$name,character(1)))
  bond <- bond_list[[num]]
  return(bond)
}


