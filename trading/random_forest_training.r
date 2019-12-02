library(xts)
library(glmnet)
library(dplyr)
source("machine-learning/random_forest.R")
source("machine-learning/combine_data.R")

random_forest_trading <- function(prices){
  rf <- random_forest(prices, type = "binary")

  testing <- combine_data(prices)
  names(testing) <- make.names(names(testing))
  trades <- predict(rf, newdata = testing)
  trades <- as.character(trades)
  trades <- as.integer(trades)
  
  trades[trades == 0] = -1
  
  vix_returns <- testing[,1]
  returns_trades <- vix_returns * trades
  
  return(returns_trades)
}


if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  data <- random_forest_trading(prices)
  
  saveRDS(data, file = "RData/rf_fit.RDS")
  
  mean <- sum(data["2017"]*.1)
  sd <- sd(data["2017"]*.1*sqrt(250))
  
  sharpe <- (mean-.02)/sd

}
