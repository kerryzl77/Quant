setwd('/Users/liuzikai/Desktop/4539')
IBEX <- read.csv(file = 'ibex202021.csv')
IBEX
portfolio <-1000000
IBEX<-na.omit(IBEX)
ret<-100*diff(log(IBEX$Close))

# a) variance forecast using RiskMetricsTM ---------------
n = length(ret)
plot(ret,type = "l")
# Squared first return as σ2ES1 
ret[1]
ES1 <- ret[1]^2
ES1
ES <- numeric(n)
ES[1] <- ES1
# σˆ2ES,t = 0.94ˆσ2ES,t−1 + 0.06r2t−1
for (t in 2:n) {
  ES[t] <- 0.94 * (ES[t - 1]) + 0.06 * (ret[t - 1]^2)
}
cat("The last ESt value is:", ES[n])
# The last ESt value is: 1.456778
0.94 * (ES[n]) + 0.06 * (ret[n]^2)
############ 
# 1.382136
############

require(fGarch)
# b) Standardize using RiskMetricsTM ---------------
std_ret <- ret / sqrt(ES)
plot(std_ret,type = "l")
# GED
ged_fit<-gedFit(std_ret)
ged_fit$par
# mean        sd        nu 
# 0.0329661 1.0948296 1.1546631 

gedFit2<-function (x, ...)
{
  start = c(mean = 0, sd = 1, nu = 2)
  loglik = function(x, y = x) {
    f = -sum(log(dged(y, 0, 1, x[3])))
    f
  }
  fit = nlminb(start = start, objective = loglik, lower = c(-Inf,
                                                            0, 0), upper = c(Inf, Inf, Inf), y = x, ...)
  names(fit$par) = c("mean", "sd", "nu")
  fit
}
ged_fit2 <- gedFit2(std_ret)
# gedFit2(std_ret)$par
# mean       sd       nu 
# 0.000000 1.000000 1.200871 
gedfit_nu <- ged_fit2$par[3][1]
# gedfit_nu <- 1.200871
alpha <- 0.99
ged_quantile <- qged(1 - alpha, nu = gedfit_nu)
# -2.64342 
ES_n1 <- 0.94 * (ES[n]) + 0.06 * (ret[n]^2)
VaR_1d_99 <- portfolio * (1- exp(sqrt(ES_n1) * ged_quantile/100))
VaR_1d_99
############ 
# 30599.24 
############ 

# c) GARCH ---------------
# Fit a GARCH(1,1) model with zero mean and GED innovations
garch_model <- garchFit(formula = ~ garch(1, 1), data = ret, cond.dist = "ged", include.mean = FALSE, trace = FALSE)
# Display the GARCH model summary
summary(garch_model)
# Coefficient(s):
#   omega   alpha1    beta1    shape  
# 0.11539  0.16673  0.79102  1.26026  
plot(residuals(garch_model))
qqnorm(residuals(garch_model))
qqline(residuals(garch_model))

# d)-------------- --------------- -------------- ---------------
summary(garch_model)
res <- (tail(residuals(garch_model),1))
variance <- tail(volatility(garch_model, type = "h"),1)
forcast <- 0.11539 + 0.16673 * res^2 + 0.79102 * variance
# Calculate the conditional variance forecast for the next trading day (h = 1)
garch_forecast <- predict(garch_model, n.ahead = 1)
garch_forecast^2  # 1.131152
forcast # 1.131154
############ 
# 1.131154
############ 


# e)-------------- --------------- -------------- ---------------
GARCH_quantile <- qged(1 - 0.99, nu = garch_model@fit$coef["shape"])
VaR_1d_GARCH <- portfolio * (1-exp(GARCH_quantile*garch_forecast/100))
VaR_1d_GARCH
#  27390.07

VaR_1d_GARCH <- portfolio * (1- exp(sqrt(forcast) * GARCH_quantile/100))
VaR_1d_GARCH
# 27390.1 

############ 
# 27390.1 
############ 


sres<-residuals(garch_model,standardize=TRUE)

par(mfrow=c(1,2))
qqnorm(sres)
qqline(sres)
plot(sres,main = "Standardized residuals")
par(mfrow=c(1,1))

# c) -------------- --------------- -------------- ---------------
library(rugarch)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                   distribution.model = "ged")
fit <- ugarchfit(spec, ret)
(fit)
# Estimate  Std. Error  t value Pr(>|t|)
# omega    0.11476    0.049952   2.2975 0.021590
# alpha1   0.16864    0.051331   3.2854 0.001018
# beta1    0.79075    0.053605  14.7515 0.000000
# shape    1.25907    0.097296  12.9405 0.000000



