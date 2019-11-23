library(dynlm)
library(zoo)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

source("machine-learning/combine_data.R")
source("machine-learning/test_error.R")

dynamic_linear <- function(prices, backtest = FALSE, sample = NULL){
  
  values <- combine_data(prices, backtest = backtest, samples = sample)
  xs <- values[,-1]
  y <- values[,1]
  linear.fit <- lm(vix_return ~ .,  data = values)
  stepwise <- stepAIC(linear.fit, direction = "both", trace = FALSE)
  
  step.coefs <- names(stepwise$coefficients)[-1]
  coefs <- gsub("`", "", step.coefs)
  coefs <- c("vix_return" , coefs)
  values.subset <- subset(values, select = coefs)
  
  dlm.fit <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + ., data = values.subset )
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
  
  linear.fit <- lm(vix_return ~ .,  data = values.train)
  stepwise <- stepAIC(linear.fit, direction = "both", trace = FALSE)
  summary(stepwise)
  
  step.coefs <- names(stepwise$coefficients)[-1]
  coefs <- gsub("`", "", step.coefs)
  coefs <- c("vix_return" , coefs)
  values.subset <- subset(values, select = coefs)
  
  dlm.fit1 <- dynlm(vix_return ~ ., data = values.subset[-seq(10),])
  dlm.fit2 <- dynlm(vix_return ~ L(vix_return, seq(1,5)) + ., data = values.subset[-seq(5),] )
  dlm.fit3 <- dynlm(vix_return ~ L(vix_return, seq(1,5)) + L(VIX, 1) + ., data = values.subset[-seq(5),] )
  dlm.fit4 <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + ., data = values.subset )
  dlm.fit5 <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + trend(vix_return) + ., data = values.subset )
  
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
