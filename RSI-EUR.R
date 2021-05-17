rm(list = ls())
require(quantstrat)
require('FinancialInstrument')
source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))
source(paste0(path.package("quantstrat"),"/demo/luxor.getSymbols.R"))

library(devtools)
library(quantmod)
library(quantstrat)
library(TTR)
library(png)
library(stats4)
library("lubridate")
library("stringr")
library("PerformanceAnalytics")

#setup
currencypair <- "EURUSD"

initDate = '2020-01-01'
.from <- initDate #start of backtest
.to <- '2020-12-31' #end of backest

data <- read.csv("ask_data.csv",header= TRUE)
data$Time <- substr(data$Time,1,nchar(data$Time)-11)
data$Time <- str_replace(data$Time, "T", " ")
data$Time <- ymd_hms(data$Time)  
EURUSD <- xts(data[,2:6],order.by=data[,1])
names(EURUSD)[names(EURUSD) == "Open"] <- paste(currencypair,"Open",sep=".")
names(EURUSD)[names(EURUSD) == "High"] <- paste(currencypair,"High",sep=".")
names(EURUSD)[names(EURUSD) == "Low"] <- paste(currencypair,"Low",sep=".")
names(EURUSD)[names(EURUSD) == "Close"] <- paste(currencypair,"Close",sep=".")
names(EURUSD)[names(EURUSD) == "Volume"] <- paste(currencypair,"Volume",sep=".")


myTheme <- chart_theme()
myTheme$col$dn.col <- 'lightblue'
myTheme$col$dn.border <- 'lightgrey'
myTheme$col$up.border <- 'lightgrey'
chart_Series(EURUSD, theme = myTheme)



#RSI lengths
n=14

# trade parameters
.threshold = 0.0005
.orderqty = 100000
.txnfees = -6 

#stop loss amount
.stoploss <- 0.30/100
.StopLoss = seq(0.05, 0.6, length.out=48)/100

#trading window
.timespan = 'T00:00/T23.59'

portfolio.st = 'forex'
account.st = 'IB1'
strategy.st = 'luxor'

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(portfolio.st, symbols = 'EURUSD', initDate = initDate, currency = 'USD')
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = 'USD' )
initOrders(portfolio.st, initDate = initDate)
strategy(strategy.st, store = TRUE)

#add indicator RSI
add.indicator(strategy.st,
              name = 'RSI',
              arguments = list(price = quote(Cl(mktdata)),
                               maType = "SMA"), label = "RSI")

#add signals. RSI generates long positions when crosses 30 and sell positions when crosses 70, exit signal the opposite
add.signal(strategy.st,
           name = "sigThreshold", arguments = list(threshold = 30, column="RSI",
                                                  relationship="lt", cross=TRUE),
           label = "long")

add.signal(strategy.st,
           name = "sigThreshold", arguments = list(threshold=70, column="RSI", relationship="gt",
                                                  cross=TRUE), label = "short")

#add long entry rule
add.rule(strategy.st,
         name = 'ruleSignal',
         arguments = list(sigcol="long", sigval=TRUE,
                          orderside='long',
                          ordertype='stoplimit', prefer='High',
                          treshold=.threshold,
                          orderqty=+.orderqty,
                          replace=FALSE
         ),
         type = 'enter',
         label='EnterLONG')

#add sell entry rule
add.rule(strategy.st,
         name = 'ruleSignal',
         arguments = list(sigcol='short', sigval=TRUE,
                          orderside='short',
                          ordertype='stoplimit', prefer='Low', treshold=.threshold,
                          orderqty=-.orderqty,
                          replace=FALSE
         ),
         type = 'enter',
         label= 'EnterSHORT')

#add exit long
add.rule(strategy.st, name = 'ruleSignal',
         arguments = list(sigcol='long', sigval=TRUE,
                          orderside='short',
                          ordertype='market',
                          orderqty='all',
                          TxnFees=.txnfees,
                          replace=TRUE
         ),
         type = 'exit',
         label = 'Exit2LONG')

#add exit short
add.rule(strategy.st, name = 'ruleSignal',
         arguments = list(sigcol='short', sigval=TRUE,
                          orderside='long',
                          ordertype='market',
                          orderqty='all',
                          TxnFees=.txnfees,
                          replace=TRUE
         ),
         type = 'exit',
         label = 'Exit2SHORT')

#add strategy
results <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st, symbols = "EURUSD")
