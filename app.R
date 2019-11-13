#rm(list=ls())

library(tibble)
library(shiny)
library(plotly)
library(xts)
source("data-visualizations/generate_surfacePlot.R")
source("trading/linear_training.R")
source("trading/logit_training.R")
source("trading/lasso_training.R")
source("trading/ridge_training.R")
source("trading/2018.R")
source("machine-learning/returns.R")
#source("generate-data/generate_all.R")

#generate() #generates data

vol <- readRDS("RData/vol.RDS")
prices <- readRDS("RData/prices.RDS")


library(ggplot2)  # for the diamonds dataset

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Barracuda"),
   
   tabsetPanel(
       tabPanel("Volatility Surface", 
                
            fluidRow(
              id = "dateContainer0",
              column(4,
                     #ticker selector for vol graph
                     selectInput("ticker", "Ticker:", names(vol), selected = "SPY")
              ),
              column(4,
                     p("Configure Additional Dates:", style = "font-weight: bold; margin: 0px 0px 5px 0px"),
                     # UX for adding a dynamic number of dates
                     actionButton("addDate", "+ Date")
              )
            ),
            
            #vol grpah
            plotlyOutput("surfacePlot")
            
          ),
       tabPanel("Prices", 
            
            #First price graph
            selectInput("graph1", "Price Graph:", names(prices), selected = "RF"),
            plotlyOutput("Plot1"),
            
            #Second price graph
            selectInput("graph2", "Price Graph:", names(prices), selected = "VIX"),
            plotlyOutput("Plot2")
                
          ),
       tabPanel("Trading Strategies",
        
            h3("General Settings"),
            sliderInput("capitalAllocation", "Capital Allocation %:",
                        width = "100%",
                        min = 0, max = 100,
                        value = 50),
            
            h3("Comparison of Returns"),
            DT::dataTableOutput("dataTable"),
            
            
            h3("Algorithm Detail"),
            numericInput("transactionCosts", "Annual Transaction Costs (% pts):", 1.0),
            
            selectInput("algo", "Algorithm:", choices = "Loading", selected = "Loading"),   
            #Linear Regression Histogram
            plotOutput("TradeHist"),
            plotlyOutput("ReturnslPlot"),
            plotlyOutput("ReturnsYearly"),
            plotlyOutput("SDYearly"),
            plotlyOutput("SharpYearly")
            
        ),
       tabPanel("2018 Backtest",
                sliderInput("capitalAllocation2018", "Capital Allocation %:",
                            width = "100%",
                            min = 0, max = 100,
                            value = 50),
                h4("Trade Returns Histogram"),
                plotOutput("TradeHistBacktest"),
                h4("Trade Returns Over Time"),
                plotlyOutput("ReturnslPlotBacktest"),
                h4("Benchmark Plot"),
                plotOutput('BenchmarkPlotBacktest')
        )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ##########################################
  ####COMMON VARIABLES AND CONFIGURATION
  ##########################################
  years <- c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
  algos <- c("Linear Regression", "Logit", "Lasso", "Ridge")
  
  observe({
    updateSelectInput(session, "algo", choices = algos)
  })
  
  ##########################################
  ####PANEL: VOLATILITY SURFACE
  ##########################################
  numDates <- 0
  dateSelectorPrefix <- "date"
  dateDescriptionPrefix <- "description"
  dateDefault <- "2008-01-10"
  
  addDate <- function(){
    numDates <<- 1 + numDates
    
    groupID <- paste0("dateContainer", numDates)
    dateSelectorID <- paste0(dateSelectorPrefix, numDates)
    dateSelectorTitle <- paste0("Date ", numDates, ":")
    descriptionID <- paste0(dateDescriptionPrefix, numDates)
    prevElementID <- paste0("dateContainer", numDates - 1)
    
    insertUI(
      selector = paste0("#", prevElementID),
      where = "afterEnd",
      ui = fluidRow(
        id = groupID,
        style = "margin-left: 30px;",
        dateInput(dateSelectorID, dateSelectorTitle, value = dateDefault),
        #the nearest available date with vol data
        textOutput(descriptionID)
      )
    )
    
    output$surfacePlot <- renderPlotly({
      
      dates <- list()
      
      count <- 1
      while(count <= numDates){
        dateSelectorID = paste0(dateSelectorPrefix, count)
        
        dateInput <- input[[dateSelectorID]]
        if(is.null(dateInput)){
          dateInput <- dateDefault
        }
        
        dates <- append(dates, as.Date(dateInput))
        
        count <- 1 + count
      }
      
      surface <- merge(vol[[input$ticker]][["18M"]], vol[[input$ticker]][["24M"]], join = "inner") 
      
      # finds the available dates in vol
      availibleDates <- as.Date(index(surface))
      goodDates <- list()
      
      # Finds the closest date we have data for
      count <- 1
      while(count <= numDates){
        goodDates <- append(goodDates, which(abs(availibleDates-dates[count]) == min(abs(availibleDates-dates[count]))))
        
        count <- 1 + count
      }
      
      cleanedDates <- list()
      
      count <- 1
      while(count <= numDates){
        cleanedDate <- toString(availibleDates[[strtoi(goodDates[count])]])
        cleanedDates <- append(cleanedDates, cleanedDate)
        
        dateDescriptionID <- paste0(dateDescriptionPrefix, count)
        #output[[dateDescriptionID]] <- renderText({c("Date Used: ", cleanedDate)})
        
        count <- 1 + count
      }
      
      #Builds the graph
      generate_surfacePlot(
        vol, 
        input$ticker, 
        cleanedDates
      )
    })
  }
  addDate()
  
  observeEvent(input$addDate, {
    addDate()
  })
  
  ##########################################
  ####PANEL: PRICES
  ##########################################
  
  #Generates the first date graph based on selected ticker
  output$Plot1 <- renderPlotly({
    asset <- input$graph1
    data <- data.frame(prices[[asset]])
    plot_ly(data, x= ~rownames(data), y= ~data[[1]], type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "", type="date", tickformat = '%Y'), yaxis = list(title = asset))
  })

  #Generates the second date graph based on selected ticker
  output$Plot2 <- renderPlotly({
    asset <- input$graph2
    data <- data.frame(prices[[asset]])
    plot_ly(data, x= ~rownames(data), y= ~data[[1]], type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "", type="date", tickformat = '%Y'), yaxis = list(title = asset))
  })
  
  ##########################################
  ####PANEL: Trading
  ##########################################
  
  getReturns <- function(algo){
    if(algo == "Linear Regression"){
      linear_trading(prices)
    }
    else if(algo == "Logit"){
      logit_trading(prices)
    }
    else if(algo == "Lasso"){
      lasso_trading(prices)
    }
    else if(algo == "Ridge"){
      ridge_trading(prices)
    }
    else if(algo == "SPY"){
      spy_prices <- prices[["SPY"]]
      
      returns("SPY", prices)
    }
  }
  
  output$TradeHist <- renderPlot({
    algo <- input$algo
    returns <- getReturns(algo)
    
    returns <- returns*input$capitalAllocation
    
    hist(returns, breaks = 100, main = "Trade Returns Histogram")
  })
  
  output$ReturnslPlot <- renderPlotly({
    algo <- input$algo
    returns <- getReturns(algo)
    data <- data.frame(returns)
    data <- data*input$capitalAllocation
    
    plot_ly(data, x= ~rownames(data), y= ~data[[1]], type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "", type="date", tickformat = '%Y'), yaxis = list(title = "Returns"))
  })
  #Interesting facts:
  #Our Correlation is .09 with S&P 500
  #Slight long bias, 60% of the time
  
  getReturnsYearly <- function(algo){
    data <- getReturns(algo)
    
    returns <- c()
    for(x in years){
      r_year <- data[x]
      
      returns <- c(returns, sum(r_year)*input$capitalAllocation)
    }
    
    returns
  }
  
  output$ReturnsYearly <- renderPlotly({
    algo <- input$algo
    
    returns <- getReturnsYearly(algo) - input$transactionCosts
    plot_ly(x= ~years, y= ~returns, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "", type="date", tickformat = '%Y'), yaxis = list(title = "Returns"))
  })
  
  #### #### #### ####
  #### STDEV
  #### #### #### ####
  
  getStdevYearly <- function(algo){
    data <- getReturns(algo)
    
    returns <- c()
    
    for(x in years){
      r_year <- data[x]
      
      num <- length(r_year)
      
      returns <- c(returns, sd(r_year*input$capitalAllocation/100)*100*sqrt(num))
    }
    
    returns
  }
  
  output$SDYearly <- renderPlotly({
    algo <- input$algo
    
    returns <- getStdevYearly(algo)
    
    plot_ly(x = years, y = returns, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "", type="date", tickformat = '%Y'), yaxis = list(title = "Standard Deviation"))
  })
  
  #### #### #### ####
  #### SHARPE RATIO
  #### #### #### ####
  getSharpeYearly <- function(algo){
    data <- getReturns(algo)
    
    sharpe <- c()
    for(x in years){
      r_year <- data[x]
      num <- length(r_year)
      #Only invest 50% to protect capital
      returns <- sum(r_year)*input$capitalAllocation - input$transactionCosts
      
      sd <- sd(r_year*input$capitalAllocation / 100)*100*sqrt(num)
      sharpe <- c(sharpe, (returns-.02)/sd)
    }
    
    sharpe
  }
  
  output$SharpYearly <- renderPlotly({
    algo <- input$algo
    
    sharpe <- getSharpeYearly(algo)
    
    plot_ly(x= ~years, y= ~sharpe, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "", type="date", tickformat = '%Y'), yaxis = list(title = "Sharpe Ratio"))
  })
  
  #### #### #### ####
  #### DATA TABLE
  #### #### #### ####
  calc_avg_return <- function(algo){
    val <- sapply(data.frame(getReturnsYearly(algo)), mean)
    val <- format(round(val, 2), nsmall = 2)
    
    paste0(formatC(val, format = "f", digits = 2), "%")
  }
  
  calc_avg_std_dev <- function(algo){
    val <- sapply(data.frame(getStdevYearly(algo)), mean)
    format(round(val, 2), nsmall = 2)
  }
  
  calc_avg_sharpe <- function(algo){
    val <- sapply(data.frame(getSharpeYearly(algo)), mean)
    format(round(val, 2), nsmall = 2)
  }

  output$dataTable <- DT::renderDataTable({
    
    avg_return <- c()
    avg_std_dev <- c()
    avg_sharpe <- c()
    
    for(x in algos){
      avg_return <- c(avg_return, calc_avg_return(x))
      avg_std_dev <- c(avg_std_dev, calc_avg_std_dev(x))
      avg_sharpe <- c(avg_sharpe, calc_avg_sharpe(x))
    }
    
    frame <- data.frame(
      algorithm = algos,
      avg_return,
      avg_std_dev,
      avg_sharpe,
      stringsAsFactors = FALSE
    )
    
    DT::datatable(frame,
                  options = list(paging = FALSE,
                                 searching = FALSE,
                                 bInfo = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = 2:4))
                                 )
                  )
    
  })
  
  ##########################################
  ####PANEL: BACKTEST
  ##########################################
  output$TradeHistBacktest <- renderPlot({
    
    Returns <- linear_trading2018(prices) 
    
    Returns <- Returns*input$capitalAllocation2018
    
    hist(Returns, breaks = 100, main = "")
  })
  
  output$ReturnslPlotBacktest <- renderPlotly({
    
    data <- data.frame(linear_trading2018(prices))
    data <- data*input$capitalAllocation2018
    
    plot_ly(data, x= ~rownames(data), y= ~data[[1]], type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "", type="date", tickformat = '%b-%Y'), yaxis = list(title = "Returns"))
  })
  
  #### #### #### ####
  #### BENCHMARK PLOT (Code modified from that provided by Jacob Vestal's make-benchmark-plot.R; availible at https://gitlab.oit.duke.edu/jmv13/problem-set-2-solution)
  #### #### #### ####
  
  output$BenchmarkPlotBacktest <- renderPlot({
    
    spy_returns <- getReturns("SPY")["2018"]
    barracuda_returns <- linear_trading2018(prices)
    
    time_basis <- "Daily"
    
    benchmark_data <- merge(
      spy_returns,
      barracuda_returns,
      join = 'inner'
    ) %>% 
      coredata() %>%
      as_tibble() %>%
      set_colnames(c("x", "y"))
    
    lm_for_trendline <- lm(benchmark_data$y ~ benchmark_data$x)
    
    ggplot(
      data = benchmark_data
    ) +
      geom_point(
        mapping = aes(
          x = x,
          y = y
        )
      ) +
      xlab(
        paste0(
          time_basis,
          " return for S&P500 (SPY) "
        )
      ) +
      ylab(
        paste0(
          time_basis,
          " return for Barracuda "
        )
      ) + 
      geom_abline(
        slope     = lm_for_trendline$coefficients[2],
        intercept = lm_for_trendline$coefficients[1],
        linetype  = "longdash", 
        color     = "#d15c79", 
        size      = 2
      ) + 
      geom_label(
        data = tibble(
          xpos = -Inf,
          ypos =  Inf,
          annotateText = paste0(
            "α = ",
            round(
              lm_for_trendline$coefficients[2], 
              digits = 3
            ),
            "; β = ",
            round(
              lm_for_trendline$coefficients[1], 
              digits = 3
            )
          ),
          hjustvar = "inward",
          vjustvar = "inward"
        ),
        aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText)
      )
    
  })
}

# Run the application 

shinyApp(ui = ui, server = server)
