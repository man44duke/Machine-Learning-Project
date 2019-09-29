library(xts)
source("machine-learning/2018_lr.R")
source("machine-learning/combine_data.R")


linear_trading2018 <- function(prices){
  lr <- linear_regression2018(prices)
  coeffs <- lr$coefficients
  coeffs[is.na(coeffs)] <- 0
  coeffs <- as.matrix(coeffs)
  values <- combine_data(prices)
  data <- values
  data[,1] <- 1
  vix_returns <- values[,1]
  
  trades <- data%*%coeffs
  
  trades[trades > 0] = 1
  trades[trades <= 0] = -1
  
  returns_trades <- merge(vix_returns, trades)
  
  trade_returns <- returns_trades[,1]*returns_trades[,2]
  
  trade_returns["2018"]
}


#Testing
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  source("machine-learning/combine_data.R")
  returns_trade <- linear_trading2018(prices)
  plot(returns_trade)
  yearly <- sum(returns_trade)
  ave <- mean(returns_trade)
  sd<- sd(returns_trade)*sqrt(length(returns_trade))
  sharpe <- (yearly-.02)/sd
}