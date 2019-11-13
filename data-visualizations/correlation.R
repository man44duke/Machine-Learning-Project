library(corrplot)
source("machine-learning/combine_data.R")

data <- combine_data(prices)

df <- data.frame(date=index(data), coredata(data))
correlation <- cor(data[,1:15])

corrplot(correlation)
