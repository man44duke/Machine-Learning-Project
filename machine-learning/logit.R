library(xts)
source("machine-learning/combine_data.R")

logit <- function(prices){
  
  values <- combine_data(prices, "binary", backtest = TRUE, samples = "train")
  xs <- values[,-1]
  y <- values[,1]
  
  fit <- glm(coredata(y) ~ coredata(xs), family = binomial)
}


##Testing 
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  glm <- logit(prices)
  summary(glm)
}