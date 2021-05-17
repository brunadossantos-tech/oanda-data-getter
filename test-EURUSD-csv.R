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


#Sys.setenv(TZ= "GMT") #Set up enviroment for timestamps
#currency(c('EUR', 'USD'))
#exchange_rate('EURUSD', tick_size = 0.0001)
currencypair <- "EURUSD"

initDate = '2020-01-03'
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




#getSymbols.FI(Symbols = "GBPUSD", dir = system.file('extdata', package = 'quantstrat'), from=.from, to=.to)

#GBPUSD = to.minutes30(GBPUSD)
#GBPUSD = align.time(GBPUSD, 1800)

myTheme <- chart_theme()
myTheme$col$dn.col <- 'lightblue'
myTheme$col$dn.border <- 'lightgrey'
myTheme$col$up.border <- 'lightgrey'
chart_Series(EURUSD, theme = myTheme, label="EMA")
addEMA(n=30,on=1,col = "blue")
addEMA(n=200,on=1,col = "red")

#EMA lengths
.fast = 12
.slow = 26

# trade parameters
.threshold = 0.0005
.orderqty = 100000
.txnfees = -6 

#stop loss amount
.stoploss <- 0.30/100
.StopLoss = seq(0.05, 0.6, length.out=48)/100

#trading window
.timespan = 'T00:00/T23.59'

portfolio.st = 'ema'
account.st = 'ema'
strategy.st <-portfolio.st

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(portfolio.st, symbols = 'EURUSD', initDate = initDate, currency = 'USD')
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = 'USD' )
initOrders(portfolio.st, initDate = initDate)
strategy("ema", store = TRUE)


#add indicator EMA
add.indicator("ema" ,
              name = 'EMA',
              arguments = list(
                x = quote(Cl(mktdata)[,1]),
                n= .fast
              ),
              label="nFast")

add.indicator("ema",
              name = 'EMA',
              arguments = list(
                x=quote(Cl(mktdata)[,1]),
                n= .slow
              ),
              label = "nSlow")

#add signals

add.signal(
  "ema",
  name = 'sigCrossover',
  arguments = list(
    columns=c("nFast", "nSlow"),
    relationship="gt"
  ),
  label = 'long'
)

add.signal(
  "ema",
  name = 'sigCrossover',
  arguments = list(
    columns=c("nFast", "nSlow"),
    relationship="lt"
  ),
  label = 'short'
)

#add long entry rule

add.rule("ema",
         name = 'ruleSignal',
         arguments = list(sigcol='long', sigval=TRUE,
                          orderside='long',
                          ordertype='stoplimit', prefer='High',
                          treshold=.threshold,
                          orderqty=+.orderqty,
                          replace=FALSE
                          ),
                        type = 'enter',
                        label='EnterLONG')

#add short entry rule
add.rule("ema",
         name = 'ruleSignal',
         arguments = list(sigcol='short', sigval=TRUE,
                          orderside='short',
                          ordertype='stoplimit', prefer='Low', treshold=.threshold,
                          orderqty=-.orderqty,
                          replace=FALSE
         ),
         type = 'enter',
         label= 'EnterSHORT')

#add long exit rules
add.rule("ema", name = 'ruleSignal',
         arguments = list(sigcol='long', sigval=TRUE,
                          orderside='short',
                          ordertype='market',
                          orderqty='all',
                          TxnFees=.txnfees,
                          replace=TRUE,
                          ),
         type = 'exit',
         label = 'Exit2LONG')

#add short exit rules
add.rule("ema", name = 'ruleSignal',
         arguments = list(sigcol='short', sigval=TRUE,
                          orderside='long',
                          ordertype='market',
                          orderqty='all',
                          TxnFees=.txnfees,
                          replace=TRUE
         ),
         type = 'exit',
         label = 'Exit2SHORT')

# apply strategy
results <- applyStrategy(strategy = "ema", portfolios = portfolio.st, symbols = "EURUSD")

#view transactions
getTxns(Portfolio = portfolio.st, Symbol = "EURUSD")

#Plot performance
chart.Posn(portfolio.st, "EURUSD",
           TA="add_EMA(n=12, col=2); add_EMA(n=26, col=4)", theme=myTheme)

ob <- getOrderBook(portfolio.st)$forex$EURUSD

View(mktdata)

#update Portfolio -  calculates the Profit & Loss for each symbol in symbols.
updatePortf(portfolio.st)

#update Account - calculates the equity from the portfolio data. 
updateAcct(account.st)

#update Equity - updates the ending equity for the account. 
updateEndEq(account.st)

#trade statistics
View(t(tradeStats('ema')))

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


