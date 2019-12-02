library(xts)
library(randomForest)
source("machine-learning/combine_data.R")

# #Hyperparamter tuning
#   mtry_error <- function(mtry_test) {
#   test_model <- randomForest(as.factor(values[,1]) ~ . , data = values, mtry=mtry_test, importance=TRUE, na.action=na.omit) #test on lti
#   
#   test_sample <- even_sample(lti_valid, 100)
#   test_sample_resp <- test_sample$inAudience
#   
#   test_pred <- predict(test_model, test_sample) #predict one lti_valid
#   
#   test_tab <- table(test_pred, test_sample_resp)
#   
#   test_mce <- 1-sum(diag(test_tab))/sum(test_tab) 
#   return(test_mce) }
# 
# mce_vec <- vector()
# 
# for (i in 3:20) {
#   mtry_sum = 0
#   for (j in 1:10) {
#     mtry_sum = mtry_sum + mtry_error(j)
#   }
#   mtry_mean = mtry_sum/10
#   mce_vec = c(mce_vec,mtry_mean)
# }
# mtry = 8


random_forest <- function(prices, type = NULL){
  
  if(!is.null(type)){
    values <- combine_data(prices, "binary", TRUE, "train")
    names(values) <- make.names(names(values))
  }
  else{
    values <- combine_data(prices, backtest = TRUE, sample = "train")
    names(values) <- make.names(names(values))
  }
  
  #xs <- values[,-1]
  #y <- values[,1]
  if(!is.null(type)){
    fit <- randomForest(as.factor(vix_return) ~ . , data = values, mtry=8)
  }
  else{
    fit <- randomForest(vix_return ~ . , data = values, mtry=8)
  }
  return(fit)
}


##Testing 
if(FALSE){
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/prices.rda")
  load("~/Financial Infromatics/barracuda/barracuda.package/barracuda.package/data/vol.rda")
  
  rf <- random_forest(prices, type = "binary")
  preds <- predict(rf, newdata = values)
  preds
  summary(rf)
}
