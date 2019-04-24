library(logspline)
library(MASS)
library(rriskDistributions)
library(fitdistrplus)

data <- read.csv("Task0.csv",header = TRUE)

hist(data$x,col = "green", lwd = 1)
fit.cont(data2fit=data$x)
#gamma, lognorm, F(оно)

#произвольно разбили пополам на test и train
d = sort(sample(nrow(data), nrow(data)*.5))
train<-data[d,]
test<-data[-d,]

est_mu <- mean(train)
est_sd <- sd(train)
est_var <- var(train)

##оценка параметра методом максимального правдоподобия
ln <- fitdistr(train, "lognormal", method = "mle")
ks.test(test,"plnorm",  ln[["estimate"]][["meanlog"]], ln[["estimate"]][["sdlog"]])

alpha = est_mu * est_mu / est_var
betta = est_mu / est_var
ks.test(test, 'pgamma', alpha, betta)

##оценка параметра методом максимального правдоподобия
gam <- fitdist(train, "gamma", method="mle")
ks.test(test, 'pgamma', gam[["estimate"]][["shape"]], gam[["estimate"]][["rate"]])

d2 <- 2 * est_mu / (est_mu - 1)
d1 <- (2*d2*d2*d2-4*d2*d2)/(est_var*(d2-2)*(d2-2)*(d2-4)-2*d2*d2)
d1 <- as.integer(round(d1))
d2 <- as.integer(round(d2))
d1
d2
ks.test(test, "pf", d1, d2)
fit <- fitdistr(train, "f", list (df1 = d1, df2 = d2))
d1 <- as.double (fit$estimate[1][1])
d2 <- as.double (fit$estimate[2][1])
ks.test(test, "pf", d1, d2)

#5 10 фишер


