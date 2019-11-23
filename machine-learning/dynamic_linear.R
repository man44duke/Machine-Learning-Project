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
  
  linear.fit <- lm(vix_return ~ .,  data = values)
  stepwise <- stepAIC(linear.fit, direction = "both", trace = FALSE)
  
  step.coefs <- names(stepwise$coefficients)[-1]
  coefs <- gsub("`", "", step.coefs)
  coefs <- c("vix_return" , coefs)
  values.subset <- subset(values, select = coefs)
  
  valuesLagged <- addLag(values.subset, "vix_return", 10)
  valuesLagged <- addLag(valuesLagged, "VIX", 1)
  
  dlm.fit <- dynlm(vix_return ~ ., data = valuesLagged )
  return(dlm.fit)
}

col <- "vix_return"

addLag <- function(values, col,  lags){
  
  names <- c(paste0(col,"L", 1))
  merged <- lag(values[,col], 1)
  if(lags > 1){
    for(i in 2:lags){
      lagCol <- lag(values[,col], i)
      merged <- cbind(merged, lagCol)
      names <- c(names, paste0(col,"L", i))
    }
  }
  
  colnames(merged) <- names
  values <- cbind(values, merged)
  
  return(values)
  
}


##Testing 
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  dlm.fit <- dynamic_linear(prices, TRUE, "train")
  summary(dlm.fit)


  values <- addLag(values.train, "vix_return", 10)

  values.train <- combine_data(prices, backtest = TRUE, samples = "train")
  values.2017 <- combine_data(prices, backtest = TRUE, samples = "train")
  
  linear.fit <- lm(vix_return ~ .,  data = values)
  stepwise <- stepAIC(linear.fit, direction = "both", trace = FALSE)
  step.coefs <- names(stepwise$coefficients)[-1]
  coefs <- gsub("`", "", step.coefs)
  coefs <- c("vix_return" , coefs)
  values.subset <- subset(values.train, select = coefs)
  
  #plot(vix_return ~ ., data = values.subset)
  
  dlm.fit1 <- dynlm(vix_return ~ ., data = values.subset[-seq(10),])
  dlm.fit2 <- dynlm(vix_return ~ L(vix_return, seq(1,5)) + ., data = values.subset[-seq(5),] )
  dlm.fit3 <- dynlm(vix_return ~ L(vix_return, seq(1,5)) + L(VIX, 1) + ., data = values.subset[-seq(5),] )
  dlm.fit4 <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + ., data = values.subset )
  dlm.fit5 <- dynlm(vix_return ~ L(vix_return, seq(1,10)) + L(VIX, 1) + trend(vix_return) + ., data = values.subset )
  
  anova(dlm.fit1, dlm.fit2, dlm.fit3, dlm.fit4, dlm.fit5)
  summary(dlm.fit4)
  
}
