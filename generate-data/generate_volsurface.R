library(xts)
library(readxl)
library(magrittr)

vol <- lapply(
  setNames(
    list.files("Data/Volatility", full.names = TRUE),
    gsub(".xlsx", "", list.files("Data/Volatility"))
  ),
  FUN = function(location){

    # Test Data:
    # location <- list.files("inst/extdata/Volatility", full.names = TRUE)[1]

    lapply(
      setNames(excel_sheets(location), excel_sheets(location)),
      FUN = function(sheet){

        read_excel(path = location, sheet = sheet) %>%
          tibble::column_to_rownames(var = colnames(.)[1]) %>%
          as.xts()

      }
    )

  }
)



