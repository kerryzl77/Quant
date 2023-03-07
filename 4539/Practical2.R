require(tseries)
require(zoo)
require(quantmod)
require(rmutil)

# a) -------
AAPL <- get.hist.quote("AAPL", start = '2019-12-02', end = '2020-12-02', quote = "Close")
MCD <- get.hist.quote("MCD", start = '2019-12-02', end = '2020-12-02', quote = "Close")
JPM <- get.hist.quote("JPM", start = '2019-12-02', end = '2020-12-02', quote = "Close")

AAPl <- na.omit(AAPL)
MCD <- na.omit(MCD)
JPM <- na.omit(JPM)

# b)
AAPL_D_Return = diff(AAPL$Close)/lag(AAPL)
MCD_D_Return = diff(MCD$Close)/lag(MCD)
JPM_D_Return = diff(JPM$Close)/lag(JPM)
Portfolio_D_Return = AAPL_D_Return * 0.4 + JPM_D_Return * 0.5 + MCD_D_Return * 0.1
attach(mtcars)
par(mfrow=c(2,2))
plot(AAPL_D_Return)
plot(MCD_D_Return)
plot(JPM_D_Return)
plot(Portfolio_D_Return)

# c) -------
AAPL_C_Return = na.omit(log(1 + AAPL_D_Return))
MCD_C_Return = na.omit(log(1 + MCD_D_Return))
JPM_C_Return = na.omit(log(1 + JPM_D_Return))
Portfolio_C_Return = na.omit(log(1 + Portfolio_D_Return))
# head(Portfolio_C_Return)

# d)
# Comparison discrete versus continuous returns: McDonalds
MCD_D_20 <- tail(MCD_D_Return, n=20)
MCD_C_20 <- tail(MCD_C_Return, n=20)
x1<-1:20

par(mfrow=c(1,1))
# Use P for point instead of line 
plot(x1,MCD_D_20, type = "p",col = 2, xlab = "Year",ylab = "Values")
lines(x1,MCD_C_20,type = "p",col = 3)
legend("topright",c("Discrete", "Continious"),lty = 1, col = 2:3)

Portfolio_C_Return_Appr <- AAPL_C_Return * 0.4 + JPM_C_Return * 0.5 + MCD_C_Return * 0.1
Portfolio_C_Return_Appr <- tail(Portfolio_C_Return_Appr, n=20)
Portfolio_C_Return_Ex <- tail(Portfolio_C_Return, n=20)
plot(x1,Portfolio_C_Return_Appr, type = "p",col = 2, xlab = "Days",ylab = "Values")
lines(x1,Portfolio_C_Return_Ex,type = "p",col = 3)
legend("topright",c("Approx", "Exact"),lty = 1, col = 2:3)

mu <- 0.005
sigma2 <- 0.0017
x <- seq(-0.02, 0.03, length.out = 100)
normal_pdf <- dnorm(x, mean = mu, sd = sqrt(sigma2))
laplace_pdf <- dlaplace(x, m = mu, s = sqrt(sigma2/2))
# laplace_pdf_discrete <- exp(laplace_pdf) -1
# laplace_pdf_discrete <- (1/1+x)*laplace_pdf
# mu1 <- mean(laplace_pdf_discrete)
# sigma2_1 <- sd(laplace_pdf_discrete)
# plot(laplace_pdf_discrete)

par(mfrow=c(1,2))
plot(normal_pdf,main = 'Normal',ylab = 'y')
plot(laplace_pdf,main = 'Laplace',ylab = 'y')

# laplace_pdf_standard <- dlaplace(x, m = 0, s = 1)
# df <- length(x) - 1
# t_crit_0.99 <- qt(0.01/2, df)
# t_crit_0.95 <- qt(0.05/2, df)
# ci_0.99 <- c(1,-1)*t_crit_0.99 * (1 / sqrt(length(x)))
# ci_0.95 <- c(1,-1)*t_crit_0.95 * (1 / sqrt(length(x)))
# ci_0.99;ci_0.95
# ci_normal_0.99 <- c(-1,1)*qnorm(0.99)
# ci_normal_0.95 <- c(-1,1)*qnorm(0.95)
# ci_normal_0.99;ci_normal_0.95

compquant<-matrix(c(qlaplace(0.01,0,1/sqrt(2)),qlaplace(0.05,0,1/sqrt(2)),
                    qnorm(0.01),qnorm(0.05)),2,2)
colnames(compquant)<-c("Laplace","Normal")
rownames(compquant)<-c("1%", "5%")
compquant

# (d) 10-day continuous returns CI
ci_cts_0.95 <- c(qlaplace(0.05, m = mu, s = sqrt(sigma2/2)),qlaplace(0.95, m = mu, s = sqrt(sigma2/2)))
ci_cts_0.99 <- c(qlaplace(0.01, m = mu, s = sqrt(sigma2/2)),qlaplace(0.99, m = mu, s = sqrt(sigma2/2)))
ci_cts_0.95; ci_cts_0.99
# 10-day Discrete returns CI
ci_disc_0.95 <- exp(ci_cts_0.95) - 1
ci_disc_0.99 <- exp(ci_cts_0.99) - 1
ci_disc_0.95; ci_disc_0.99

# e)

VaR<- -ci_disc_0.99[1]*10^6
MRC<-max(VaR, 3.65*56000) # The PF is 0.65 when the overshootings are 7

VaR;MRC
# MRC is 204400



