library(xts)
library(glmnet)
source("machine-learning/ridge.R")
source("machine-learning/combine_data.R")


ridge_trading <- function(prices){
  ridge <- ridge(prices)
  opt <- ridge$lambda.min
  fit <- ridge$glmnet.fit
  values <- combine_data(prices, backtest = TRUE, sample = "test")
  data <- values
  data <- data[,-1]
  vix_returns <- values[,1]
  
  trades <- predict(fit, newx = coredata(data), s = opt)
  
  trades[trades > 0] = 1
  trades[trades <= 0] = -1
  
  returns_trades <- merge(vix_returns, trades)
  
  trade_returns <- returns_trades[,1]*returns_trades[,2]
}


#Testing
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  
  data <- ridge_trading(prices)
  returns <- c()
  years <- c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  for(x in years){
    r_year <- data[x]
    returns <- c(returns, sum(r_year)*.5)
  }
  
  mean(returns)
  sd(returns)
}