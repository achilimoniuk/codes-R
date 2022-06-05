install.packages("openxlsx")
rm(list=ls())
library(openxlsx)
data <- read.xlsx("C:/Users/Acer/Desktop/raport metody/model.xlsx")

#wizualizacja danych
hist(data$index)
data$lindex <- log(data$index)
hist(data$income)
hist(data$unemp)
data$lunemp <- log(data$unemp)
hist(data$lunemp)

hist(data$pollution)
data$lpollution <- log(data$pollution)
hist(data$homicide)
data$lhomicide <- log(data$homicide)
hist(data$lhomicide)
hist(data$votes)
hist(data$corruption)

data$pollution2 <- data$pollution^2
plot(data$income,data$index)
plot(data$lunemp,data$index)
plot(data$educ,data$index)
data$educ2 <- data$educ^2
#MNK
#model pierwszy
model <- lm(index~income+lunemp+educ+pollution+lhomicide+votes+corruption+US,data=data)
summary(model)
cor(data[c("income","lunemp","educ","pollution","lhomicide","votes","corruption","US")])
vif(model)
library(car)
#model drugi
model1 <- lm(index~income+lunemp+educ+pollution+lhomicide+votes+corruption,data=data)
summary(model1)
vif(model1)

#model ostateczny
library(stargazer)
model2 <- lm(index~income+lunemp+pollution+corruption,data=data)
summary(model2)
stargazer(model, model2, type="text")
vif(model2)

#test RESET
library(lmtest)
reset(model2)

#normlanosc skladnika losowego
res <- model2$residuals
require(moments)
S <- skewness(res)
K <- kurtosis(res)

hist(model2$residuals, breaks = 20, xlab = "Reszty z modelu", main = "Histogram reszt" )
plot(density(res))


library(tseries)
jarque.bera.test(res)

#heteroskedastycznosc
data$res2 <- model$residuals^2
plot(data$res2)
plot(data$income,data$res2, main="zale¿noœæ miêdzy kwadratem reszt a zmienn¹ income")
plot(data$lunemp,data$res2,main="zale¿noœæ miêdzy kwadratem reszt a zmienn¹ lunemp")
plot(data$pollution,data$res2, main="zale¿noœæ miêdzy kwadratem reszt a zmienn¹ pollution")
plot(data$corruption,data$res2,main="zale¿noœæ miêdzy kwadratem reszt a zmienn¹ corruption")

bptest(model)

model2 <- lm(index~income+lunemp+pollution+corruption,data=data)
model_white	= lm(res2~income+lunemp+pollution+corruption+I(income^2)+I(lunemp^2)+I(pollution^2)+I(corruption^2)+I(income*lunemp)+I(income*pollution)+I(income*corruption)+I(lunemp*pollution)+I(lunemp*corruption)+I(pollution*corruption),data=data)
LM_white <- nrow(data)*summary(model_white)$r.squared
summary(model_white)
pchisq(LM_white,14,lower.tail = F) 

#UMNK
auxliary <- lm(log(res2)~income+lunemp+pollution+corruption,data=data)
data$weights <- 1/exp(auxliary$fitted.values)
wls <- lm(index~income+lunemp+pollution+corruption,data=data, weights= weights)
bptest(wls)
#odporne bledy standardowe
library(sandwich)
VCOVHC <- vcovHC(model2,type="HC3")
coef =coeftest(model2, vcov.=VCOVHC)


library(stargazer)
stargazer(model2,wls,coef,type="text")

#ENDOGENICZNOSC
library(AER)
IV <- ivreg(index~income+lunemp+pollution+corruption|US+lunemp+pollution+corruption, data=data)
summary(IV)
stargazer(model2, IV, type='text')
#test Walda dla pierwszej restrykcji
fst <- lm(income~US+lunemp+pollution+corruption, data=data)
linearHypothesis(fst, "US=0")
#test Walda dla drugiej restrykcji
ols2 <-  lm(index~income+lunemp+pollution+corruption+US,data=data)
linearHypothesis(ols2, "US=0")
summary(ols2)
#test Hausmana
IV <- ivreg(index~income+lunemp+pollution+corruption|US+lunemp+pollution+corruption, data=data)
summary(IV, diagnostics=T)
