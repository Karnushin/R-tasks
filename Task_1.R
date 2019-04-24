library(fGarch)
library(LaplacesDemon)
library(MASS)
library(TSA)
library(LaplacesDemon)
library(nortest)

s <- read.csv("Task1.csv",header = TRUE)
head(s)
regr <-  lm(s$y ~ s$x1 + s$x2 + s$x3, data=s)
summary(regr)

regr <-  lm(s$y ~ s$x3, data=s)
summary(regr)
plot(regr$fitted.values)

plot(regr$residuals ,type = "b",col = 'red', main = 'Residuals')
hist(regr$residuals,col = "red")

# равномерное, нормальное N(0,σ2), Лапласа с параметрами 0,μ
#t-распределение Стьюдента с параметром df - число степеней свободы
#f-stat - if these variables are jointly significant (have effect on y)
#гипотеза,что все коэф = 0 (те ли х подходят или не те)
#R how well your model fits the data
#M R-sq - корреляционная связь между 1сл в и нек мно-ом сл в (иксы влияют на у)
#какая доля дисперсии результативного признака y объясняется влиянием факторных признаков xi
resid.mean = mean(regr$residuals)
resid.std = sd(regr$residuals)
print(resid.mean)
print(resid.std)

# оба на нормальность
shapiro.test(regr$residuals)
pearson.test(regr$residuals)

ks.test(regr$residuals, "pnorm", resid.mean, resid.std)
ks.test(regr$residuals, "plaplace",resid.mean,sqrt(2)/resid.std)
ks.test(regr$residuals, "pt", 2 * resid.std/(resid.std-1))
#получаем расп Стьюдента!!!!!!!!

#Колм-Смир на проверку Стьюдента
df = 2 * resid.std/(resid.std-1)
stud = rt(length(regr$residuals), df)
ks.test(regr$residuals, stud)

#КС на норма-ть
nor = rnorm(length(regr$residuals),resid.mean, resid.std)
ks.test(regr$residuals, nor)

#не коррелируют
acf(regr$residual)

