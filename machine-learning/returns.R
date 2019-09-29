library(xts)

returns <- function(tick, prices){
  price <- prices[[tick]]
  return <- price/lag(price)
  return <- return[-1]
  return <- lapply(return, function(x) {x-1})[[1]]
}