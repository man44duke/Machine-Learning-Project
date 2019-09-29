library(xts)
library(boot)
library(caret)

source("machine-learning/returns.R")

#Note: if the type is something execpt for "NULL" then the function returns a binary (up/down) value
combine_data <- function(prices, type=NULL, backtest = FALSE, samples = NULL){
  
  vix_returns <- returns("VIX", prices)

  if(!is.null(type)){
    binary <- lapply(vix_returns, function(x) as.numeric(x > 0))[[1]]
    coredata(vix_returns) <- binary
  }
  
  vix_vol <- vol$SPY[[1]]
  #vix_vol <- prices$RF
  
  for (x in vol$SPY){
    vix_vol <- merge(vix_vol, x, join = "inner")
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
    smp_size <- floor(.35 * nrow(values))
    set.seed(123)
    sample <- sample.int(n=nrow(values), size = smp_size, replace = F)
    if(samples == "test"){
      test <- values[-sample,]
      return(test)
    }
    if(samples == "train"){
      train <- values[sample,]
      return(train)
    }
    
  }
  values
  
}
