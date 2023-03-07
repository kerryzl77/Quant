install.packages('tseries') 
install.packages('zoo') 
install.packages('quantmod')
library(tseries)
library(quantmod)

# get.hist.quote('tseries') 
# tseries::get.hist.quote


# require(tseries)
# require(zoo) -- 使用require 然后直接索引function
# dji <- get.hist.quote("^DJI", start = "1992-01-01", end = "2022-01-24", quote = "Close")
# par(mfrow=c(1,2)) 两个side-by-side 作图
par(mfrow=c(1,1)) 


help(get.hist.quote)
DJI <- tseries::get.hist.quote(instrument = "^dji", start = "1992-01-01", end = "2022-01-24",
               quote = c( "Close"),) 
# time series starts 1992-01-02
# time series ends   2022-01-21
plot(DJI,xaxt = 'n',, xlab="Time")
axis.Date(1, at=seq(as.Date("1992-01-01"), as.Date("2022-01-24"),  
                    by="5 years"))

# ------------参考答案-----------
# Get the dates
dates<-index(DJI)
# Note the year
year<-substr(dates,1,4)
# Check when the year changes
year1<-year[-1]
year2<-year[-length(year)]
ind<-which(year1 !=year2)
ind
# This will give you the time point of the last observation in the respective year
year[ind]
startoftheyear<-c(1,ind[c(5, 10, 15, 20, 25, 30)]+1)
year[startoftheyear]
plot(as.numeric(DJI), type="l", xaxt="n",cex.lab=1.4,cex.axis=1.4, xlab="Time", ylab="DJI", main="Closing proces for the Dow Jones Index")
axis(1,at=startoftheyear,label=year[startoftheyear], cex.axis=1.4)




# help(getSymbols)
require(quantmod)
SSE <- getSymbols("SSE.L", from="2022-11-01", to="2022-11-30", src="yahoo")
tail(SSE.L,3)
chartSeries(SSE.L,subset = 'last 12 months',type = 'candlesticks')
# chartSeries(SSE.L, type="candlesticks", theme="white", subset='2020-11-01::2020-11-30')
zooom(n=1,eps=2)
help(getSymbols)

