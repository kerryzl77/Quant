require(tseries)
require(quantmod)
require(e1071)
require(tidyverse)

# discrete returns 251 closing prices for the S&P500 index ending on the 30.12.2008
start_date <- as.Date("2008-01-01")
end_date <- as.Date("2008-12-30")
sp500 <-get.hist.quote(instrument="^GSPC",start=start_date,end=end_date, quote="Close")
Rfun<-function(x) (x[-1]-x[-length(x)])/x[-length(x)]
dsp500<-Rfun(as.numeric(sp500))
plot.ts(dsp500, main="retun in %")

# Historical simulation for VAR 99% = 3/250
sr1<-sort(100*as.numeric(dsp500)) 
sr1[3]
-sr1[3]*10^4 # 88067.76
-sqrt(10)*sr1[3]*10^4 # 278494.7

# rt1 <- mean(dsp500) - 2.3263 * sd(dsp500)
# Rt1 <- exp(rt1) - 1
# 
# initial_investment <- 1000000
# Var1 <- -Rt1 * initial_investment
# Var10 <- sqrt(10)*Var1
# Var1;Var10
# # [1] 60032.2
# # [1] 189838.5

start_date2 <- as.Date("2015-01-01")
end_date2 <- as.Date("2015-12-30")
sp500_2015 <-get.hist.quote(instrument="^GSPC",start=start_date2,end=end_date2, quote="Close")
dsp500_2015<-Rfun(as.numeric(sp500_2015))
sr3<-sort(100*as.numeric(dsp500_2015))
sr3[3]  # -2.957645

-sr3[3]*10^4  #  29576.45
-sqrt(10)*sr3[3]*10^4 # 93528.94

# rt2 <- mean(dsp500_2015) - 2.3263 * sd(dsp500_2015)
# Rt2 <- exp(rt2) - 1
# Var2 <- -Rt2 * initial_investment
# Var10_2 <- sqrt(10)*Var2
# Var2;Var10_2
# # [1] 22431.37
# # [1] 70934.22

############################################################################################################
# Exercise 2

start_date3 <- as.Date("2020-01-07")
end_date3 <- as.Date("2020-12-31")
Siemens <-get.hist.quote(instrument="SIE.DE",start=start_date3,end=end_date3, quote="Close")
Allianz <-get.hist.quote(instrument="ALV.DE",start=start_date3,end=end_date3, quote="Close")
SAP <-get.hist.quote(instrument="SAP.DE",start=start_date3,end=end_date3, quote="Close")
BMW <-get.hist.quote(instrument="BMW.DE",start=start_date3,end=end_date3, quote="Close")
dSiemens <- 100*Rfun(as.numeric(Siemens))
dAllianz <- 100*Rfun(as.numeric(Allianz))
dSAP <- 100*Rfun(as.numeric(SAP))
dBMW <- 100*Rfun(as.numeric(BMW))
mean = c(mean(dSiemens),mean(dAllianz),mean(dSAP),mean(dBMW))
stock_stats <- data.frame(stock = c("Siemens", "Allianz", "SAP", "BMW"),
                          mean = c(mean(dSiemens),mean(dAllianz),mean(dSAP),mean(dBMW)),
                          sd = c(sd(dSiemens), sd(dAllianz), sd(dSAP), sd(dBMW)),
                          skewness = c(skewness(dSiemens), skewness(dAllianz), skewness(dSAP), skewness(dBMW)),
                          kurtosis = c(kurtosis(dSiemens), kurtosis(dAllianz), kurtosis(dSAP), kurtosis(dBMW)),
                          min = c(min(dSiemens), min(dAllianz), min(dSAP), min(dBMW)),
                          max = c(max(dSiemens), max(dAllianz), max(dSAP), max(dBMW)),
                          quantile_1 = c(quantile(dSiemens, 0.01), quantile(dAllianz, 0.01), quantile(dSAP, 0.01), quantile(dBMW, 0.01)),
                          quantile_5 = c(quantile(dSiemens, 0.05), quantile(dAllianz, 0.05), quantile(dSAP, 0.05), quantile(dBMW, 0.05)),
                          quantile_95 = c(quantile(dSiemens, 0.95), quantile(dAllianz, 0.95), quantile(dSAP, 0.95), quantile(dBMW, 0.95)),
                          quantile_99 = c(quantile(dSiemens, 0.99), quantile(dAllianz, 0.99), quantile(dSAP, 0.99), quantile(dBMW, 0.99)))
stock_stats

weight <- c(1000,1600,700,400)
Siemens_value = Siemens * weight[1]
Allianz_value = Allianz * weight[2]
SAP_value = SAP * weight[3]
BMW_value = BMW * weight[4]
portfolio_value = Siemens_value + Allianz_value + SAP_value + BMW_value

Siemens_weight = Siemens_value / portfolio_value
Allianz_weight = Allianz_value / portfolio_value
SAP_weight = SAP_value / portfolio_value
BMW_weight = BMW_value / portfolio_value

data <- (data.frame(      time_data1 = Siemens_weight$Close,
                               time_data2  = Allianz_weight$Close,
                               time_data3  = SAP_weight$Close,
                               time_data4  = BMW_weight$Close))

plot(data$time_data1,type = "l", col = 2, xlab = "Day",ylab = "Weight",ylim = c(0, 1))
lines(data$time_data2,type = "l",col = 3)
lines(data$time_data3,type = "l",col = 4)
lines(data$time_data4,type = "l",col = 5)
legend("topright",c("Siemens", "Allianz", "SAP","BMW"),lty = 1,col = 2:5)

# par(mfrow=c(2,2))
# plot.ts(dSiemens, main="Siemens")
# plot.ts(dAllianz, main="Allianz")
# plot.ts(dSAP, main="SAP")
# plot.ts(dBMW, main = "BMW")
# par(mfrow=c(1,1))
