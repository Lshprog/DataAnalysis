install.packages('nortest')
install.packages('corrplot')
library(nortest)
library(corrplot)


#Видалення недоцільної статистики
remove_unc <- function(x,upb,lwb){
  data <- subset(data,x < upb & x > lwb)
}

data <- remove_unc(data$gp,90,10)

#Створення матриці зі стовпчиками які аналізуються
dataframe <- data[,c(3,6,7)]

#Перевірка на нормальність хи-квадарт Пірсона
print(pearson.test(dataframe$player_height))
print(pearson.test(dataframe$pts))
print(pearson.test(dataframe$reb))

#Перевірка на нормальність Андерсона-Дарлінга
print(ad.test(dataframe$player_height))
print(ad.test(dataframe$pts))
print(ad.test(dataframe$reb))

#Пошук рангового коефіцієнта кореляції Спірмена та p-value
print(cor.test(dataframe$player_height,dataframe$reb,method = "spearman",exact = FALSE))
print(cor.test(dataframe$player_height,dataframe$pts,method = "spearman",exact = FALSE))
print(cor.test(dataframe$pts,dataframe$reb,method = "spearman",exact = FALSE))

#Пошук рангового коефіцієнта кореляції Кендела та p-value
print(cor.test(dataframe$player_height,dataframe$reb,method = "kendall",exact = FALSE))
print(cor.test(dataframe$player_height,dataframe$pts,method = "kendall",exact = FALSE))
print(cor.test(dataframe$pts,dataframe$reb,method = "kendall",exact = FALSE))

#Матриця парних зв`язків
corrplot(cor(dataframe,y=NULL,method = "spearman"),method = 'number')
corrplot(cor(dataframe,y=NULL,method = "kendall"),method = 'number')

pairs(~player_height+pts+reb,data = dataframe,
      main = "Scatterplot Matrix",col = "black",pch=19)

#функція отримання p-value із статистики
extract_p<- function (modelobject) {
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#функція виведення потрібної інформації на екран
print_out_ps <- function(x,y,s,t,l){
  cat("Залежна змінна - ",s,"    Незалежні змінні- ",t,",", l,"\n")
  cat("Коефіцієнт детермінації: ",x,"\n")
  cat("p-value: ",y,"\n")
  
}

#створення регресійних моделей
model1 <- lm(dataframe$player_height ~ dataframe$pts + dataframe$reb,data = dataframe)
model2 <- lm(dataframe$pts ~ dataframe$player_height + dataframe$reb,data = dataframe)
model3 <- lm(dataframe$reb ~ dataframe$pts + dataframe$player_height,data = dataframe)

#вивід на екран
print_out_ps(summary(model1)$adj.r.squared,extract_p(model1),"player_height","pts","reb")
print_out_ps(summary(model2)$adj.r.squared,extract_p(model2),"pts","player_height","reb")
print_out_ps(summary(model3)$adj.r.squared,extract_p(model3),"reb","player_height","pts")





