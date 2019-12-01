library(xts)
library(glmnet)

source("machine-learning/combine_data.R")

ridge <- function(prices){
  
  values <- combine_data(prices, backtest = TRUE, samples = "train")
  xs <- values[,-1]
  y <- values[,1]
  lambdas <- 10^seq(3, -2, by = -.1)
  
  fit <- cv.glmnet(y = coredata(y), x=coredata(xs), alpha = 0, lambda = lambdas)
}


##Testing 
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  ridge <- ridge(prices)
  plot(ridge)
}