rm(list = ls())
require(quantstrat)
require('FinancialInstrument')
#source(paste0(path.package("quantstrat"),"/demo/luxor.include.R"))
#source(paste0(path.package("quantstrat"),"/demo/luxor.getSymbols.R"))

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

#setup BB
maType='SMA'
n = 20
sdp = 2

#add indicator
add.indicator(strategy.st, 
              name = "BBands", 
              arguments = list(HLC = quote(HLC(mktdata)), 
                               n=n, 
                               maType=maType, 
                               sd=sdp 
              ), 
              label='BBands')

#add signals short
add.signal(strategy.st,
           name="sigCrossover",
           arguments = list(columns=c("Close","up"),
                            relationship="gt"),
           label="Cl.gt.UpperBand")

#add signals long
add.signal(strategy.st,
           name="sigCrossover",
           arguments = list(columns=c("Close","dn"),
                            relationship="lt"),
           label="Cl.lt.LowerBand")

#add signals for exit
add.signal(strategy.st,name="sigCrossover",
           arguments = list(columns=c("High","Low","mavg"),
                            relationship="op"),
           label="Cross.Mid")

#add rules for entry short
add.rule(strategy.st, name='ruleSignal', 
         arguments = list(sigcol="Cl.gt.UpperBand",
                          sigval=TRUE, 
                          orderqty=-.orderqty, 
                          ordertype='stoplimit', prefer='Low', treshold=.threshold,
                          orderside='short', 
                          replace=FALSE),
         type='enter',
         label = "EntryShort")

#add rule entry Long
add.rule(strategy.st,name='ruleSignal', 
                        arguments = list(sigcol="Cl.lt.LowerBand",
                                         sigval=TRUE, 
                                         orderqty=+.orderqty, 
                                         ordertype='stoplimit', prefer='High',
                                         treshold=.threshold,
                                         orderside='long', 
                                         replace=FALSE),
                        type='enter',
                        label = "EntryLong")

#add rule exit trade
add.rule(strategy.st,name='ruleSignal',
                        arguments = list(sigcol="Cross.Mid",
                                         sigval=TRUE, 
                                         orderqty= 'all', 
                                         ordertype='market', 
                                         orderside=NULL, 
                                         TxnFees=.txnfees,
                                         replace=TRUE),
                        type='exit',
                        label ='exit')


#add strategy
results <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st, symbols = "EURUSD")

#view transactions
getTxns(Portfolio = portfolio.st, Symbol = "EURUSD")

#Plot performance
chart.Posn(portfolio.st, "EURUSD",
           TA="add_BBands(on = 1, sd = 1.6, n = 20)", theme=myTheme)

ob <- getOrderBook(portfolio.st)$forex$EURUSD

View(mktdata)

#update Portfolio -  calculates the Profit & Loss for each symbol in symbols.
updatePortf(portfolio.st)

#update Account - calculates the equity from the portfolio data. 
updateAcct(account.st)

#update Equity - updates the ending equity for the account. 
updateEndEq(account.st)

#trade statistics
View(t(tradeStats('forex')))

perTradeStats(portfolio.st)

tstats <- tradeStats(Portfolios = portfolio.st)

tstats[, 4:ncol(tstats)] <- round(tstats[, 4:ncol(tstats)],2)
print(data.frame(t(tstats[,-c(1,2)])))

#chart Maximum adverse excursion
chart.ME(Portfolio = portfolio.st, Symbol = "EURUSD", type = 'MAE', scale = 'percent')

#chart Maximum favorable excursion
chart.ME(Portfolio = portfolio.st, Symbol = 'EURUSD', type = 'MFE', scale = 'percent')

#account summary
a <- getAccount(account.st)

library(lattice)
xyplot(a$summary,type="h",col=4)
