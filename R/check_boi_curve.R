#' Check results against Bank Of Israel zero curves data
#'
#' A function to check the differences between the curves calculated by the package and the curves
#' calculated by the Bank Of Israel
#' @param result A list with the results of the package calculation
#' @param boi_file a csv file with the zero curves data from the bank of israel
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @return A list with the model data, BOI data and differences

check_boi <- function(result,boi_file) {
  #get BOI data
  BOI <- read.csv(boi_file,stringsAsFactors = FALSE)
  #Check Terms
  terms <- as.numeric(substr(colnames(BOI)[-1],2,3))
  #terms <- c(1,2,3,4,5,7,10,15,20)
  end_file <- length(terms) + 1
  names(BOI)[1] <- "date"
  names(BOI)[2:end_file] <- terms
  BOI$date <- as.Date(BOI$date)
  #get end of month data from result
  model <- result$model
  a <- result$result
  a <- filter(a,date %in% BOI$date)
  #calculate the curves for each date
  if (model=="NS") {
    num <- 4
  } else if (model=="NSS") {
    num <- 6
  }
  model_yields <- apply(a,1,function(x) calc_yields(terms,as.numeric(x[2:(2+num-1)]),model=model))
  model_yields <- t(model_yields)
  model_yields <- data.frame(date=format(a$date,"%Y-%m-%d"),model_yields*100)
  model_yields$date <- as.Date(model_yields$date)
  names(model_yields)[2:end_file] <- terms
  BOI <- filter(BOI,date %in% model_yields$date)
  dif <- BOI
  dif[2:end_file] <- BOI[2:end_file]-model_yields[2:end_file]
  maxs <- apply(dif,1,function(x) as.numeric(max(x[2:end_file])))
  maxs <- data.frame(date=BOI$date,max_difference=maxs)
  list(dif=dif,BOI=BOI,model=model_yields,maxs=maxs)
}
