library(xts)
library(glmnet)
source("machine-learning/dynamic_linear.R")
source("machine-learning/combine_data.R")

dlm_trading <- function(prices){
  dlm <- dynamic_linear(prices, TRUE, "train")
  
  testing <- combine_data(prices)
  testing <- addLag(testing, "vix_return", 10)
  testing <- addLag(testing, "VIX", 1)
  testing <- testing[-seq(10),]
  
  vix_returns <- testing[,1]
  
  trades <- predict(dlm, newdata = testing)

  trades[trades > 0] = 1
  trades[trades <= 0] = -1
  
  returns_trades <- vix_returns * trades
  
  return(returns_trades)
}


#Testing
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  data <- dlm_trading(prices)
  
  mean <- sum(data["2017"]*.1)
  sd <- sd(data["2017"]*.1*sqrt(250))
  
  sharpe <- (mean-.02)/sd
  ############
  returns_dlm <- c()
  years <- c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
  for(x in years){
    r_year <- data[x]
    returns_dlm <- c(returns_dlm, sum(r_year)*.1)
  }

  mean(returns_dlm)
  sd(returns_dlm)
}