library(TSA)
library(lmtest)
#спектральная плотность lec9

data <- read.csv("Task2.csv")
d <- ts(data[,3],start = c(2013,1), frequency = 12)
#видно непостоянство среднего, из чего делаем вывод о нестационарности временного ряда
plot(d, type = 'l', col = 'blue', xlab = 'Time', ylab = 'Value')
#делаем ряд стационарным
X <- diff(d)
plot.ts(X,col = "blue",lwd = 2,type = "l")
#acf-> MA, pacf-> AR, см кол-во 1-х выступ-х палочек
#MA 4,5,8,10,12,17
acf(X,lwd = 5, col = "blue")
#AR 4,5,8
pacf(X,lwd = 5, col = "blue")
# попробуем смеш модель (мин сумм индекса, где 0) ARMA(1,1)
eacf(X)

#model <- auto.arima(X) для авто нахождения арма модели
#summary(model)
#Сравним модели с помощью теста Льюинг-Бокса.
#Основная гипотеза:остатки являются случайными (то есть представляют собой белый шум)
###################################################
ar <- arima(X,order = c(8,0,0),method="CSS")
ar$sigma2 #Оценка методом максимального правдоподобия
ar$coef
coeftest(ar) # t-test оценок модели
#chi(X) square statistic is a measurement of how expectations compare to results
Box.test(ar$residuals, lag = 12, type = "Ljung-Box", fitdf = 2)
qqnorm(ar$residuals)
qqline(ar$residuals)
acf(ar$residuals, lwd = 5, col = "blue")

###################################################
#оценка спектральной плотности
phi <- 0.24900904
y <-arima.sim(model = list(ar = phi),n=length(X))
sp <- spec(y,col = "blue",lwd = 2,xlab= "frequency",ylab = "spectral density")
lines(sp$freq,ARMAspec(model = list(ar = phi),freq = sp$freq,plot= "F")$spec,lty = 1)
#выборочная спектральная плотность не являетя состоятельной оценкой 
#теоретической спектральной плотности.Увеличение длины выборки не приведет к более гладкой оценке. 
#поэтому (построим ниже)
#cглаженная перидограмма по окну Даниэла 5
k=kernel('daniell',c(5))
sp=spec(y,kernel=k,log='no',sub='',xlab='Frequency',
        ylab='Smoothed Sample Spectral Density',col = "blue",lwd = 2)
lines(sp$freq,ARMAspec(model=list(ar=phi),freq=sp$freq,
                       plot=F)$spec,lty=1)
lines(sp$freq,sp$spec,lwd = 2,col = "red")
legend("topleft",c("Daniell window","True spectrum","Periodogram"),bty="n",lwd = 2,col = c("red","black","magenta"))
per <- periodogram(y,plot = FALSE)
lines(per$freq,per$spec,col = "magenta")
###################################################
ma <- arima(X,order = c(0,0,12),method="CSS")
ma$coef
resma <- ma$residuals
plot.ts(resma,col = "blue",lwd = 2,type = "l", main = "residuals")

coeftest(ma)
Box.test(resma, lag = 12, type = "Ljung-Box", fitdf = 2)
qqnorm(ma$residuals)
qqline(ma$residuals)
acf(ma$residuals, lwd = 5, col = "blue")

#######################################################
#ARMA(1,0,1)
ar_mix <- arima(X,order = c(2,0,3),method="CSS")
coeftest(ar_mix)
plot.ts(ar_mix$residuals,col = "blue",lwd = 2,type = "l", main = "residuals")
qqnorm(ar_mix$residuals)
qqline(ar_mix$residuals)
Box.test(ar_mix$residuals, lag = 6, type = "Ljung-Box", fitdf = 2)
#######################################################

#Сравним модели по тесту Акаике.
ml_ar <- arima(X,order = c(12,0,0),method="ML")
ml_ar$aic
acf(ml_ar$residuals,lwd = 5, col = "blue")

ml_ma <-  arima(X,order = c(0,0,10),method="ML")
ml_ma$aic
acf(ml_ma$residuals,lwd = 5, col = "blue")
m <- arima(X,order = c(2,0,3),method="ML")
m$aic

#делаем вывод по наим val$aic AR(8) (ml_ar) наиб предпочтительна

