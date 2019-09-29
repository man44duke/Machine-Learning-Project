library(xts)
library(randomForest)
source("machine-learning/combine_data.R")

random_forest <- function(prices, type = NULL){
  
  if(!is.null(type)){
    values <- coredata(combine_data(prices, "binary"))
  }
  else{
    values <- coredata(combine_data(prices))
  }
  
  #xs <- values[,-1]
  #y <- values[,1]
  if(!is.null(type)){
    fit <- randomForest(as.factor(values[,1]) ~ . , data = values)
  }
  else{
    fit <- randomForest(values[,1] ~ . , data = values)
  }
  
}


##Testing 
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  
  rf <- random_forest(prices, type = "binary")
  summary(rf)
}