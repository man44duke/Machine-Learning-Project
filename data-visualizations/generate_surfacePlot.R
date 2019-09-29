library(plotly)

# vol: vol object in .rda format, ticker: string, dates: list of strings formtted as YYYY-MM-DD

# x-axis: moneyness slice
# y-axis: yearly volatility
# z-axis: TTE - time to expiry

generate_surfacePlot <- function(vol, ticker, dates){
  
  require(plotly)
  require(xts)
  require(magrittr)
  
  # Make a blank plot object to assign surfaces to.
  vol_plot_3d <- plot_ly(showscale = FALSE, height = 600, width = 800)
  
  lapply(
    names(vol[[ticker]]),
    FUN = function(tte){
      # tte <<- tte # for debugging
      
      vol[[ticker]][[tte]][unlist(unique(dates))] %>% # this is an XTS object
      as.data.frame() %>% # the data frame from the XTS
      tibble::rownames_to_column("date") %>% # converts the row names to a column named 'date'
      tibble::add_column(tte) # add a collumn to the end to specify the time to expiry
    }
  ) %>%
  purrr::reduce(rbind) %>%
  split(.$date) %>%
  lapply(
    FUN = function(surface_data){
      # surface_data <<- surface_data # for debugging
      
      tibble::remove_rownames(surface_data) %>%
      tibble::column_to_rownames(var = "tte") %>%
      dplyr::select(-date) %>%
      as.matrix() %>%
      t()
    }
  ) %>%
  purrr::walk(
    .f = function(surface){ # this is a dynamic "for loop for pipes"
      vol_plot_3d <<- add_surface(vol_plot_3d, z = surface)
    }
  )
  
  # Finally, return `vol_plot_3d` with the layout you want:
  vol_plot_3d %>%
  layout(
    p = vol_plot_3d,
    title = '',
    scene = list(
      xaxis = list(
        title = 'Expiration Duration',
        tickmode = "array",
        tickvals = 0:(length(names(vol[[1]])) - 1),
        ticktext = names(vol[[1]])
      ),
      yaxis = list(
        title = 'Moneyness',
        tickmode = "array",
        tickvals = 0:(length(colnames(vol[[1]][[1]])) - 1),
        ticktext = colnames(vol[[1]][[1]])
      ),
      zaxis = list(
        title = 'Implied Volatiliy'
      )
    )
  )
  
  # For Debugging:
  # save(ticker, file = "ticker.RData")
  # save(dates, file = "dates.RData")
  # save(dates, file = "vol.RData")
  
  # load("ticker.RData")
  # load("dates.RData")
  # load("vol.RData")
}

#For Testing Purposes
if(FALSE){
  location <- paste0("data/", "vol.rda") #Issue #11 need to make a consistant working directory
  load(location)
  ticker <- "SPY"
  dates <- list("2004-10-05", "2004-10-05", "2006-10-18", "2006-10-18", "2006-01-11", "2007-01-12")
  generate_surfacePlot(vol, ticker, dates)

  #date = "2005-01-10"
  #surface_generation(ticker, "2005-01-10")
}
