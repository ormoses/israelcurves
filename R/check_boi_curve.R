#' Check results against Bank Of Israel zero curves data
#'
#' A function to check the differences between the curves calculated by the package and the curves
#' calculated by the Bank Of Israel
#' @param result A list with the results of the package calculation
#' @param boi_file a csv file with the zero curves data from the bank of israel
#' @importFrom utils read.csv
#' @return A list with the model data, BOI data and differences

check_boi <- function(result,boi_file) {
  #get BOI data
  BOI <- read.csv(boi_file,stringsAsFactors = FALSE)
  terms <- c(1,2,3,4,5,7,10,15)
  names(BOI)[1] <- "date"
  names(BOI)[2:9] <- terms
  BOI$date <- as.Date(BOI$date)
  #get end of month data from result
  model <- result$model
  a <- result$result
  a <- dplyr::filter(a,date %in% BOI$date)
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
  names(model_yields)[2:9] <- terms
  BOI <- dplyr::filter(BOI,date %in% model_yields$date)
  dif <- BOI
  dif[2:9] <- BOI[2:9]-model_yields[2:9]
  maxs <- apply(dif,1,function(x) as.numeric(max(x[2:9])))
  maxs <- data.frame(date=BOI$date,max_difference=maxs)
  list(dif=dif,BOI=BOI,model=model_yields,maxs=maxs)
}
