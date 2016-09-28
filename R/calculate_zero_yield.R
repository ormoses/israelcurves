#' Calculate a yield curve using a model
#'
#' A function that calculates yields for a vector of terms using a model (Nelson Siegel or Svensson)
#' @param maturities a numeric vector of positive maturities
#' @param params a numeric vector. The model parameters. A 4-length for Nelson Siegel (NS) model and a 6-length for Svensson (NSS).
#' @param model A string indicating the model to use - "NS" for Nelson Siegel and "NSS" for Svensson.
#' @return A numeric vector of yields matching each number in the maturities vector.
#' @export
calc_yields <- function(maturities,params,model="NS") {
  if (any(maturities<0)) stop("maturities has to be positive")
  if (model=="NS") {
    if (length(params)!=4) stop("for NS model 4 parameters are expected")
    return(NS_yield(maturities,params))
  } else if (model=="NSS") {
    if (length(params)!=6) stop("for NSS model 6 parameters are expected")
    return(NSS_yield(maturities,params))
  }  else stop("please choose either NS of NSS for a model")
}



NS_yield <- function(maturities,beta) {
#beta is a vector of the parameters b0,b1,b2 and lambda

  #Check that beta is numeric
  if (class(beta)!="numeric") stop("beta has to be numeric")
  #Calculate
  first_part <- (1 - exp(-maturities / beta[4])) / (maturities / beta[4])
  NS <- beta[1]+beta[2]*first_part+beta[3]*(first_part-exp(-maturities/beta[4]))
  return(NS/100)
}

# A function that calculates yields for a vector of terms using Nelson-Siegel-Svensson (Diebold-Li)
NSS_yield <- function(maturities,beta) {
  #beta is a vector of the parameters b0,b1,b2,b3 and lambda1,lambda2

  #Check that beta is numeric
  if (class(beta)!="numeric") stop("beta has to be numeric")
  #Calculate
  first_part <- (1 - exp(-maturities / beta[5])) / (maturities / beta[5])
  second_part <- (1 - exp(-maturities / beta[6])) / (maturities / beta[6])

  NS_part <- beta[1]+beta[2]*first_part+beta[3]*(first_part - exp(-maturities / beta[5]))
  NSS <- NS_part+beta[4]*(second_part-exp(-maturities / beta[6]))
  return(NSS/100)
}

