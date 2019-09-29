library(xts)

source("machine-learning/combine_data.R")

linear_regression2018 <- function(prices){
  
  values <- combine_data(prices, backtest = TRUE, sample = "train")["20040101/20180101"]
  xs <- values[,-1]
  y <- values[,1]
  
  fit <- lm(coredata(y) ~ coredata(xs))
}


##Testing 
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  lm <- linear_regression(prices)
  summary(lm)
}
