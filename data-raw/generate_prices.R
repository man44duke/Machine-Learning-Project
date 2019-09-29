library(xts)
library(readxl)
library(magrittr)



prices <- lapply(

  setNames(
    list.files("Data/Price", full.names = TRUE),
    gsub(".xlsx", "", list.files("Data/Price"))
  ),
  FUN = function(location){
    #location <- paste0("barracuda.package/inst/extdata/",ticker, ".xlsx")

    excel <- as.data.frame(read_excel(location))
    return(xts(excel[,-1], order.by = excel[,1]))
  }
)

