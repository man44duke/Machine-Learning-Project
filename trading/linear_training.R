library(xts)
source("machine-learning/linear_regression.R")
source("machine-learning/combine_data.R")


linear_trading <- function(prices){
  lr <- linear_regression(prices)
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
}


#Testing
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  source("machine-learning/combine_data.R")
  data <- linear_trading(prices)
  returns <- c()
  years <- c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  for(x in years){
    r_year <- data[x]
    returns <- c(returns, sum(r_year)*.5)
  }
  
  mean(returns)
  sd(returns)

}