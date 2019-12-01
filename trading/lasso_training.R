library(xts)
library(glmnet)
source("machine-learning/lasso.R")
source("machine-learning/combine_data.R")


lasso_trading <- function(prices){
  lasso <- lasso(prices)
  values <- combine_data(prices)
  data <- values
  data <- data[,-1]
  vix_returns <- values[,1]
  
  trades <- predict(lasso, newx = coredata(data), s = lasso$lambda.min)
  
  trades[trades > 0] = 1
  trades[trades <= 0] = -1
  
  returns_trades <- merge(vix_returns, trades)
  
  trade_returns <- returns_trades[,1]*returns_trades[,2]
}


#Testing
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  data <- lasso_trading(prices)
  returns_lasso <- c()
  years <- c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  for(x in years){
    r_year <- data[x]
    returns_lasso <- c(returns_lasso, sum(r_year)*.5)
  }
  
  mean(returns_lasso)
  sd(returns_lasso)
}