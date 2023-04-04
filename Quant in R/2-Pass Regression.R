library(tseries)
library(quantmod)
library("readxl")
library(zoo)


setwd('/Users/liuzikai/Desktop/4539')
Book10 <- read_excel("Book10.xlsx")
head(Book10)

risk_free_rate <- 0.02

stocks_excess_returns <- Book10
for (stock in 2:(ncol(Book10)-1)) {
  stocks_excess_returns[, stock] <- Book10[, stock] - risk_free_rate
}

betas <- numeric(ncol(Book10) - 1)

for (stock in 2:(ncol(Book10)-1)) {
  first_pass_regression <- lm(stocks_excess_returns[[stock]] ~ stocks_excess_returns$`HPR FTSE100`)
  # Regress every stock to the index 
  betas[stock - 1] <- coef(first_pass_regression)[2]
}
avg_excess_returns <- apply(stocks_excess_returns[, -1], 2, mean)

second_pass_regression <- lm(avg_excess_returns ~ betas)
summary(second_pass_regression)
# Intercept (Alpha) = 0.06850
# Beta = 0.04608 sensitivity of the stock's excess return to the market


# second-pass regression suggests that there isn't a statistically significant relationship 
# between the betas and the average excess returns for the stocks in the dataset

