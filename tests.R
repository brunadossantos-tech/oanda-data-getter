library("quantmod")
library("TTR")
library("lubridate")
library("stringr")
library("PerformanceAnalytics")

data <- read.csv("bid_ask_data.csv")
data$Time <- substr(data$Time,1,nchar(data$Time)-11)
data$Time <- str_replace(data$Time, "T", " ")
data$Time <- ymd_hms(data$Time)  
head(data)
data <- xts(data[,2:6],order.by=data[,1])

Open <- Op(data)   #Open Price
High <- Hi(data)    # High price
Low <- Lo(data)  # Low price
Close<- Cl(data)   #Close Price
Volume <- Vo(data)


#EMA
ema12 <-EMA(Cl(data),n=12)
ema26 <- EMA(Cl(data), n=26)

barChart(data,
            subset='2021-01-03 22:00:00::2021-02-01 14:30:00',
            theme=chartTheme('black'))
addEMA(n = 12, col = 'blue')
addEMA(n = 26, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('data','EMA12','EMA26'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

#trading signals: BUY=1, SELL=-1, Nothing=0
#EMA 12 and 26 crossover signal

ema_EUR <- Lag(
  ifelse(Lag(ema12) < Lag(ema26) & ema12 > ema26,1,
         ifelse(Lag(ema12)>Lag(ema26)& ema12<ema26,-1,0)))
ema_EUR[is.na(ema_EUR)] <- 0

#EMA crossover strategy holds-1 dont own-0

ema_EUR_strat <- ifelse(ema_EUR > 1,0,1)
for (i in 1: length(Cl(data))){
  ema_EUR_strat[i] <- ifelse(ema_EUR[i] == 1,1, 
                             ifelse(ema_EUR[i] == -1,0, ema_EUR_strat[i-1]))
}
ema_EUR_strat[is.na(ema_EUR_strat)] <-1
ema_EUR_stratcomp <- cbind(ema12, ema26, ema_EUR, ema_EUR_strat)
colnames(ema_EUR_stratcomp) <- c('EMA(12)', 'EMA(26)', 'EMA Signal', 'EMA Position')


#Bollinger Bands

myBBands <- function (price,n,sd){
  mavg <- SMA(price,n)
  sdev <- rep(0,n)
  N <- nrow(price)
  for (i in (n+1):N){
    sdev[i]<- sd(price[(i-n+1):i])
  }
  sdev <- sqrt((n-1)/n)*sdev
  up <- mavg + sd*sdev
  dn <- mavg - sd*sdev
  pctB <- (price - dn)/(up - dn)
  output <- cbind(dn, mavg, up, pctB)
  colnames(output) <- c("dn", "mavg", "up", 
                        "pctB")
  return(output)
}

bb <-myBBands(Cl(data),n=20,sd=2)
tail(bb,n=5)

chartSeries(data,
            subset='2021-01-03 22:00:00::2021-02-01 14:30:00',
            theme=chartTheme('white'))
addBBands(n=20,sd=2)

#MACD

macd <- MACD(Cl(data), nFast=12, nSlow=26,
             nSig=9, percent=FALSE)
tail(macd,n=5)

chartSeries(data,
            subset='2021-01-03 22:00:00::2021-02-01 14:30:00',
            theme=chartTheme('white'))
addMACD(fast=12,slow=26,signal=9,type="EMA")

#RSI

rsi <- RSI(Cl(data), SMA, n=14)

myRSI <- function (price,n){
  N <- length(price)
  U <- rep(0,N)
  D <- rep(0,N)
  rsi <- rep(NA,N)
  Lprice <- Lag(price,1)
  for (i in 2:N){
    if (price[i]>=Lprice[i]){
      U[i] <- price[i]- Lprice[i]
    } else{
      D[i] <- Lprice[i]- price[i]
    }
    if (i>n){
      AvgUp <- mean(U[(i-n+1):i])
      AvgDn <- mean(D[(i-n+1):i])
      rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
    }
  }
  rsi <- reclass(rsi, price)
  return(rsi)
}

rsi <- myRSI(Cl(data), n=14)
tail(rsi,n=3)

chartSeries(data,
            subset='2021-01-03 22:00:00::2021-02-01 14:30:00',
            theme=chartTheme('white'))
addRSI(n=14,maType="SMA")

strategy(name="luxor", store=TRUE)

