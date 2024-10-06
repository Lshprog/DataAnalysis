install.packages("lmtest")
install.packages('olsrr')
install.packages('corrplot')
library("lmtest")
library(olsrr)
library(corrplot)


#Матрична діаграма
pairs(list(dataframe$reb, dataframe$pts, dataframe$player_height),col = "black",pch=19,
      labels = list("Rebounds", "Points", "P Height"))

#Побудова моделі
model = lm(dataframe$reb ~ dataframe$player_height + dataframe$pts)
summary(model)

#Перевірка залишків на нормальність
ols_test_normality(model$residuals)
qqnorm(model$residuals, ylab = "Residuals")
qqline(model$residuals, col = "red", lwd = 2)

#Довірчі інтервали
confint(model)

#Перевірка на гетероскедастичність 
bptest(model)
plot(fitted(model), model$residuals)
abline(0,0)


#Загальний графік
plot(x = predict(model),y = dataframe$reb,
     xlab = " Expected number of rebounds",ylab="Actual number of rebounds",
     main = "Expected vs Actual")
abline(0,1)

