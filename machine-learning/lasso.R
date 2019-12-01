library(xts)
library(glmnet)

source("machine-learning/combine_data.R")

lasso <- function(prices){
  
  values <- combine_data(prices, backtest = TRUE, samples = "train")
  xs <- values[,-1]
  y <- values[,1]
  
  fit <- glmnet(y = coredata(y),  x = coredata(xs))
}


##Testing 
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  lasso <- lasso(prices)
  summary(lasso)
}