# Functions to create zerocurve class and methods
# And also functions to validate that the curves are reasonable

#' Create an object of a zerocurve class
#'
#' A function that creates an object from zerocurve class - A list consisting of:
#' \enumerate{
#'  \item model - The name of the model ("NS" or "NSS")
#'  \item params - The model parameters
#'  }
#' @param model a string representing the model name.
#' @param params a numeric vector with the model fitted parameters.
#' @return An object of type "zerocurve"
#' @export
zerocurve <- function(model, params) {

  #check that params is a numeric vector
  if (is.numeric(params)==FALSE) stop("params has to be a numeric vector")
  #Check parameters fits the model
  if (model=="NS" & length(params)!=4) {
    stop("For NS model number of parameters should be 4")
  } else if (model=="NSS" & length(params)!=6) {
    stop("For NS model number of parameters should be 6")
  }

  thecurve <- list(model=model, params=params)
  class(thecurve) <- "zerocurve"
  return(thecurve)
}

#' Plot method for zerocurve class
#'
#' Plot a curve of zerocurve class.
#' @param x,y a curve object (class "zerocurve").
#' @inheritParams plot.bond
#' @importFrom ggplot2 ggplot aes geom_line geom_text theme_classic theme scale_x_date labs ggtitle
#' @export
plot.zerocurve <- function(x,y=NULL,...) {
  terms <- seq(0.1,30,by=0.1)
  yield <- calc_yields(terms,x$params,x$model)
  curve_data <- data.frame(terms=terms,yield=yield)
  ggplot(data=curve_data,aes(x=terms,y=yield))+geom_line()
}

#' Spread plot of a time series of curves
#'
#' The function plots a spread plot given a time series of curves (result of the package calculation)
#' and two terms (min and max).
#' @param result list of results from the package calculation
#' @param min_term a number. The lower term for the spread calculation
#' @param max_term a number. The higher term for the spread calculation
#' @return A plot
plot_spreads <- function(result,min_term,max_term) {
  model <- result$model
  if (model=="NS") {
    slots <- 2:5
  } else if (model=="NSS") {
    slots <- 2:7
  }
  spread_min <- apply(result$result,1,function(x) calc_yields(min_term,as.numeric(x[slots]),model))
  spread_max <- apply(result$result,1,function(x) calc_yields(max_term,as.numeric(x[slots]),model))
  thespread <- (spread_max-spread_min)*10000
  mins_5 <- order(thespread)[1:5]
  maxs_5 <- order(-thespread)[1:5]
  extr <- c(mins_5,maxs_5)
  spread <- data.frame(date=as.Date(result$result$date),min_term=spread_min,max_term=spread_max,spread=thespread)

  ggplot(data=spread,aes(x=date,y=spread))+geom_line()+
    geom_text(data=spread[extr,],aes(x=date,y=spread,label=paste0(date,", ",round(spread)))) +
    labs(x="Date",y="Spread (BP)")+
    ggtitle(paste0(max_term,"-",min_term," Spread"))
}
