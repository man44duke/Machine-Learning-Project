

test_error <- function(predictions, testData){
  preds <- predictions
  test <- testData
  mse <- mean((preds - test)^2)
  
  rmse <- (mean((preds - test)^2))^.5
  
  mean.absolute <- mean(abs(preds - test))
  
  return(c(mse, rmse, mean.absolute))
}

classifictation_error <- function(predictions, testData){
  preds <- predictions
  test <- testData
  
  nums <- preds + test
  
  tp <- sum(nums == 2)
  tn <- sum(nums == 0)
  
  nums <- preds - test
  fp <- sum(nums == 1)
  fn <- sum(nums == -1)
  
  precision <- tp/(tp+fp)
  recal <- tp/(tp+fn)
  accurracy <- (tp+tn)/(tp+tn+fp+fn)
  
  #smoke <- matrix(c(tp, fp, fn, tn),ncol=2,byrow=TRUE)
  #smoke <- as.table(smoke)
  #print(smoke)
  
  
  return(c(precision, recal, accurracy))
}
