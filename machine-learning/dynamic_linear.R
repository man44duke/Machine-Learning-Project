library(dynlm)
library(zoo)

source("machine-learning/combine_data.R")

dynamic_linear <- function(prices){
  
  values <- combine_data(prices, backtest = TRUE, samples = "test")
  xs <- values[,-1]
  y <- values[,1]
  
  dlm.fit <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + trend(vix_return) +., data = values )
  return(dlm.fit)
}

##Testing 
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  dlm.fit <- dynamic_linear(prices)
  summary(dlm.fit)


  
  dlm.fit <- dynlm(vix_return ~ ., data = values )
  dlm.fit1 <- dynlm(vix_return ~ L(vix_return, seq(1,5)) + ., data = values )
  dlm.fit2 <- dynlm(vix_return ~ L(vix_return, seq(1,5)) + L(VIX, 1) + ., data = values )
  dlm.fit3 <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + ., data = values )

  
  
  summary(dlm.fit1)
  }