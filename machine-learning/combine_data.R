library(xts)
library(boot)
library(caret)

source("machine-learning/returns.R")
vol <- readRDS("RData/vol.RDS")
prices <- readRDS("RData/prices.RDS")

#Note: if the type is something execpt for "NULL" then the function returns a binary (up/down) value
combine_data <- function(prices, type=NULL, backtest = FALSE, samples = NULL){
  
  vix_returns <- returns("VIX", prices)

  if(!is.null(type)){
    binary <- lapply(vix_returns, function(x) as.numeric(x > 0))[[1]]
    coredata(vix_returns) <- binary
  }

  vix_vol <- vol$SPY[[1]]
  #vix_vol <- prices$RF
  name <- paste(names(vol$SPY[[1]]), ".", names(vol$SPY[1]), sep ="")
  i = 2
  for (x in vol$SPY[2:6]){
    vix_vol <- merge(vix_vol, x, join = "inner")
    name <- c(name, paste(names(vol$SPY[[i]]), ".", names(vol$SPY[i]), sep =""))
    i = i+1
  }
  
  vix_vol <- align.time(vix_vol,n=86400)
  
  for (y in prices) {
    y <- align.time(y, n=86400)
    vix_vol <- merge(y, vix_vol, join = "inner")
  }
  
  #Note: must be run twice to ensure dates line up correctly,  quirk of xts align.time
  vix_returns <- align.time(vix_returns,n=86400)
  vix_returns <- align.time(vix_returns,n=86400)
  
  values <- merge(vix_returns, vix_vol, join = "inner")
  
  if(backtest){
    
    if(samples == "train"){
      test <- values["/2016"]
      values = test
    }
    if(samples == "test"){
      train <- values["2017"]
      values = train
    }
    
  }
  
  names <- c("vix_return", rev(names(prices)),name)
  
  colnames(values) = c(names)
  return(values)
  
}
