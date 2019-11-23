library(dynlm)
library(zoo)

source("machine-learning/combine_data.R")
source("machine-learning/test_error.R")

dynamic_linear <- function(prices, backtest = FALSE, sample = NULL){
  
  values <- combine_data(prices, backtest = backtest, samples = sample)
  xs <- values[,-1]
  y <- values[,1]
  
  dlm.fit <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + trend(vix_return) +., data = values )
  return(dlm.fit)
}

##Testing 
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  dlm.fit <- dynamic_linear(prices, TRUE, "train")
  summary(dlm.fit)


  values.train <- combine_data(prices, backtest = TRUE, samples = "train")
  values.2017 <- combine_data(prices, backtest = TRUE, samples = "train")
  
  dlm.fit1 <- dynlm(vix_return ~ ., data = values.train[-seq(10),] )
  dlm.fit2 <- dynlm(vix_return ~ L(vix_return, seq(1,5)) + ., data = values.train[-seq(5),] )
  dlm.fit3 <- dynlm(vix_return ~ L(vix_return, seq(1,5)) + L(VIX, 1) + ., data = values.train[-seq(5),] )
  dlm.fit4 <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + ., data = values.train )
  dlm.fit5 <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + trend(vix_return) + ., data = values.train )
  
  pred.fit1 <- predict(dlm.fit1, newdata = values.2017)
  pred.fit2 <- predict(dlm.fit2, newdata = values.2017)
  pred.fit3 <- predict(dlm.fit3, newdata = values.2017)
  pred.fit4 <- predict(dlm.fit4, newdata = values.2017)
  
  test_error(pred.fit1, values.2017$vix_return)
  test_error(pred.fit2, values.2017$vix_return)
  test_error(pred.fit3, values.2017$vix_return)
  test_error(pred.fit4, values.2017$vix_return)
  
  anova(dlm.fit1, dlm.fit2, dlm.fit3, dlm.fit4, dlm.fit5)
  summary(dlm.fit4)
}
