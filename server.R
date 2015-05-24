# Load EU Stock market data
data(EuStockMarkets)
library(lubridate)
library(timeSeries)

# Return a time window corresponding with the user's date selection
# note that an adjustment must be applied to convert calendar dates to
# an approximate business day representation
getPerfWindow <- function(beginDate, endDate) {
  yrB <- year(beginDate)
  numWeeksB <- trunc(yday(beginDate)/7) # calculate approx. num of weeks
  bizDaysB <- yday(beginDate)-numWeeksB*2 # subtract off approx. num of weekends
  
  yrE <- year(endDate)
  numWeeksE <- trunc(yday(endDate)/7)
  bizDaysE <- yday(endDate)-numWeeksE*2
  
  # create window from EuStockMarkets dataset
  outTS <- window(EuStockMarkets, start = c(yrB,bizDaysB), end = c(yrE,bizDaysE))
  
  return(outTS)
  
}

# Return a summary table that displays the total return, average daily return,
# standard deviation of daily returns and avg returns / stdev for each index
# and for a combination of the indices. Requires input of a time series and
# a vector of the index tickers.
getPerfStats <- function(mktData, tickers=c('DAX','SMI','CAC','FTSE')) {
  
  # calc daily returns
  r <- returns(mktData[,tickers])
  r <- cbind(r,rowMeans(r, na.rm=T)) # add total return column
  
  # calc cumulative returns
  cr <- mktData[nrow(mktData),tickers]/mktData[1,tickers] - 1 
  cr <- c(cr,mean(cr)) # add portfolio mean cumulative return column
  
  # create data frame to store output
  df <- data.frame()
  df <- rbind(df, cr*100)
  df <- rbind(df, colMeans(r, na.rm=T)*100) # mean returns
  df <- rbind(df, colSds(r)*100) # std dev of returns
  df <- rbind(df, df[2,]/df[3,]) # mean returns divided by std of returns
  row.names(df) <- c('Cumulative Returns (%)','Average Daily Returns (%)',
                     'Standard Deviation of Daily Returns (%)', 'Avg. Daily Returns / Std. Dev')
  names(df) <- c(tickers,'Total Portfolio')
  
  return(df)
}

# use market data to determine the cumulative return of an evenly-distributed portfolio
getPortVal <- function(mktData, tickers=c('DAX','SMI','CAC','FTSE'), initialVal) {
  # calc cumulative returns
  cr <- mean(mktData[nrow(mktData),tickers]/mktData[1,tickers])
  portVal <- cr*initialVal 
  return(portVal)
}

# create index of portfolio returns based on input market data
getPortfolioSeries <- function(mktData, tickers=c('DAX','SMI','CAC','FTSE'), initialVal) {
  # calculate mean portfolio returns
  r <- rowMeans(returns(mktData[,tickers]))
  
  # calculate cumulative retuns
  portfolio <- c(initialVal)
  for (i in 2:length(r)) {
    portfolio <- c(portfolio, portfolio[i-1]*(1+r[i]))
  }
  
  # format portfolio series as a time series to simplify plotting
  portSeries <- ts(portfolio, start = start(mktData), end = end(mktData), 
                      frequency = frequency(mktData), names = 'Portfolio Value')
  
  return(portSeries)
  
}

shinyServer(
  function(input, output) {
    output$oBusDays <- renderPrint({nrow(getPerfWindow(input$startDate, input$endDate))})
    output$oPerfTable <- renderTable({getPerfStats(getPerfWindow(input$startDate, input$endDate), 
                                                   input$idx)})
    output$oPortVal <- renderPrint({getPortVal(getPerfWindow(input$startDate, input$endDate), 
                                               input$idx, input$investAmt)})
    output$idxPlot <- renderPlot({plot(getPerfWindow(input$startDate, input$endDate)[,input$idx], 
                                       main='Selected Index Performance')})
    output$portPerfPlot <- renderPlot({plot(getPortfolioSeries(getPerfWindow(input$startDate, 
                                                                             input$endDate),
                                                               input$idx,
                                                               input$investAmt), 
                                            main='Investment Portfolio Value', ylab='',
                                            col = 'blue')})
  }
)