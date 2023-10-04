library("data.table")
library(aplpack)
library(psych)

#зчитуємо стовпці з .csv файлу
data <- fread('C:/Users/onisa/OneDrive/Documents/uni_files/sheva/DA_term4/season_2021_full.csv', 
              select = c("player_name","age","player_height","player_weight","gp","pts","reb"))

remove_outliers <- function(x,vrb){
  q1 = quantile(x,probs = 0.25)
  q3 = quantile(x,probs = 0.75)
  iqr = q3 - q1
  data <- subset(data,vrb < q3 + 1.5*iqr & vrb > q1 - 1.5*iqr)
}
#видалення аномальних значень
data <- remove_outliers(sort(data[["player_height"]]),data$player_height)
data <- remove_outliers(sort(data[["player_weight"]]),data$player_weight)
data <- remove_outliers(sort(data[["age"]]),data$age)
data <- remove_outliers(sort(data[["gp"]]),data$gp)
data <- remove_outliers(sort(data[["reb"]]),data$reb)
data <- remove_outliers(sort(data[["pts"]]),data$pts)

p_height <-sort(data[["player_height"]])
p_weight <- sort(data[["player_weight"]])
p_age <- sort(data[["age"]])
p_gp <- sort(data[["gp"]])
p_reb <- sort(data[["reb"]])
p_pts <- sort(data[["pts"]])

print(p_height)
print(p_weight)

#полігон частот
hist(p_height)
hist_1 <- hist(p_height)
lines(hist_1$counts ~ hist_1$mids,col=2,lwd=2)

hist(p_weight)
hist_2 <- hist(p_weight)
lines(hist_2$counts ~ hist_2$mids,col=2,lwd=2)

hist(p_age)
hist_3 <- hist(p_age)
lines(hist_3$counts ~ hist_3$mids,col=2,lwd=2)

hist(p_gp)
hist_4 <- hist(p_gp)
lines(hist_4$counts ~ hist_4$mids,col=2,lwd=2)

hist(p_reb)
hist_5 <- hist(p_reb)
lines(hist_5$counts ~ hist_5$mids,col=2,lwd=2)

hist(p_pts)
hist_6 <- hist(p_pts)
lines(hist_6$counts ~ hist_6$mids,col=2,lwd=2)

#скриньки з вусами
boxplot(p_height,main="p_height",xlab="Height",col=5,border=4,horizontal = TRUE,notch=TRUE)
boxplot(p_weight,main="p_weight",xlab="Weight",col=5,border=4,horizontal = TRUE,notch=TRUE)
boxplot(p_age,main="p_age",xlab="Age",col=5,border=4,horizontal = TRUE,notch=TRUE)
boxplot(p_gp,main="p_gp",xlab="Games played",col=5,border=4,horizontal = TRUE,notch=TRUE)
boxplot(p_reb,main="p_reb",xlab="Rebounds grabbed",col=5,border=4,horizontal = TRUE,notch=TRUE)
boxplot(p_pts,main="p_pts",xlab="Points earned",col=5,border=4,horizontal = TRUE,notch=TRUE)


#підрахунок вибіркових значень

#мінімального
min_p_age <- min(p_age)
min_p_weight <- min(p_weight)
min_p_height <- min(p_height)
min_p_gp <- min(p_gp)
min_p_reb <- min(p_reb)
min_p_pts <- min(p_pts)

#максимального
max_p_age <- max(p_age)
max_p_weight <- max(p_weight)
max_p_height <- max(p_height)
max_p_gp <- max(p_gp)
max_p_reb <- max(p_reb)
max_p_pts <- max(p_pts)

#медіани
med_p_age <- median(p_age)
med_p_weight <- median(p_weight)
med_p_height <- median(p_height)
med_p_gp <- median(p_gp)
med_p_reb <- median(p_reb)
med_p_pts <- median(p_pts)

#квартилі
quart_p_age <- quantile(p_age, probs = c(0.25,0.5,0.75))
quart_p_weight <- quantile(p_weight, probs = c(0.25,0.5,0.75))
quart_p_height <- quantile(p_height, probs = c(0.25,0.5,0.75))
quart_p_gp <- quantile(p_gp, probs = c(0.25,0.5,0.75))
quart_p_reb <- quantile(p_reb, probs = c(0.25,0.5,0.75))
quart_p_pts <- quantile(p_pts, probs = c(0.25,0.5,0.75))

#децилів
decil_p_age <- quantile(p_age, probs=seq(0.1,1, by=0.1))
decil_p_weight <- quantile(p_weight, probs=seq(0.1,1, by=0.1))
decil_p_height <- quantile(p_height, probs=seq(0.1,1, by=0.1))
decil_p_gp <- quantile(p_gp, probs=seq(0.1,1, by=0.1))
decil_p_reb <- quantile(p_reb, probs=seq(0.1,1, by=0.1))
decil_p_pts <- quantile(p_pts, probs=seq(0.1,1, by=0.1))


print_out_mqd <- function(x,y,z,b,n){
  cat("Мінімум: ",x,"\n")
  cat("Максимум: ",y,"\n")
  cat("Медіана: ",z,"\n")
  cat("Квартилі:\n")
  print(b)
  cat("Децилі:\n")
  print(n)
  
}

print_out_mqd(min_p_height,max_p_height,med_p_height,quart_p_height,decil_p_height)
print_out_mqd(min_p_weight,max_p_weight,med_p_weight,quart_p_weight,decil_p_weight)
print_out_mqd(min_p_age,max_p_age,med_p_age,quart_p_age,decil_p_age)
print_out_mqd(min_p_gp,max_p_gp,med_p_gp,quart_p_gp,decil_p_gp)
print_out_mqd(min_p_reb,max_p_reb,med_p_reb,quart_p_reb,decil_p_reb)
print_out_mqd(min_p_pts,max_p_pts,med_p_pts,quart_p_pts,decil_p_pts)



#математичне сподівання
mean_p_age <- mean(p_age)
mean_p_weight <- mean(p_weight)
mean_p_height <- mean(p_height)
mean_p_gp <- mean(p_gp)
mean_p_reb <- mean(p_reb)
mean_p_pts <- mean(p_pts)

#середнє геометричне
gm_p_age <- exp(mean(log(p_age)))
gm_p_weight <- exp(mean(log(p_weight)))
gm_p_height <- exp(mean(log(p_height)))
gm_p_gp <- exp(mean(log(p_gp)))
gm_p_reb <- exp(mean(log(p_reb)))
gm_p_pts <- exp(mean(log(p_pts)))

#середнє гармонічне
hm_p_age <- harmonic.mean(p_age,zero=TRUE)
hm_p_weight <- harmonic.mean(p_weight,zero=TRUE)
hm_p_height <- harmonic.mean(p_height,zero=TRUE)
hm_p_gp <- harmonic.mean(p_gp,zero=TRUE)
hm_p_reb <- harmonic.mean(p_reb,zero=TRUE)
hm_p_pts <- harmonic.mean(p_pts,zero=TRUE)

#функція моди
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#мода
mode_p_age <- getmode(p_age)
mode_p_weight <- getmode(p_weight)
mode_p_height <- getmode(p_height)
mode_p_gp <- getmode(p_gp)
mode_p_reb <- getmode(p_reb)
mode_p_pts <- getmode(p_pts)


print_out_mgh <- function(x,y,z,n){
  cat("Математичне сподівання: ",x,"\n")
  cat("Середнє геометричне: ",y,"\n")
  cat("Середнє гармонічне: ",z,"\n")
  cat("Мода: ", n, "\n")
}

print_out_mgh(mean_p_height,gm_p_height,hm_p_height,mode_p_height)
print_out_mgh(mean_p_weight,gm_p_weight,hm_p_weight,mode_p_weight)
print_out_mgh(mean_p_age,gm_p_age,hm_p_age,mode_p_age)
print_out_mgh(mean_p_gp,gm_p_gp,hm_p_gp,mode_p_gp)
print_out_mgh(mean_p_reb,gm_p_reb,hm_p_reb,mode_p_reb)
print_out_mgh(mean_p_pts,gm_p_pts,hm_p_pts,mode_p_pts)



#Характеристики розсіювання

#дисперсія
var_p_age <- var(p_age)
var_p_weight <- var(p_weight)
var_p_height <- var(p_height)
var_p_gp <- var(p_gp)
var_p_reb <- var(p_reb)
var_p_pts <- var(p_pts)

#стандартне відхилення
sd_p_age <- sqrt(var_p_age)
sd_p_weight <- sqrt(var_p_weight)
sd_p_height <- sqrt(var_p_height)
sd_p_gp <- sqrt(var_p_gp)
sd_p_reb <- sqrt(var_p_reb)
sd_p_pts <- sqrt(var_p_pts)

#коефіцієнт варіації
cv_p_age <- sd_p_age/mean_p_age
cv_p_weight <- sd_p_weight/mean_p_weight
cv_p_height <- sd_p_height/mean_p_height
cv_p_gp <- sd_p_gp/mean_p_gp
cv_p_reb <- sd_p_reb/mean_p_reb
cv_p_pts <- sd_p_pts/mean_p_pts

#ймовірнесне відхилення
cd_p_age <- 1/2*(quantile(p_age,probs = 0.75)-quantile(p_age,probs = 0.25))
cd_p_weight <- 1/2*(quantile(p_weight,probs = 0.75)-quantile(p_weight,probs = 0.25))
cd_p_height <- 1/2*(quantile(p_height,probs = 0.75)-quantile(p_height,probs = 0.25))
cd_p_gp <- 1/2*(quantile(p_gp,probs = 0.75)-quantile(p_gp,probs = 0.25))
cd_p_reb <- 1/2*(quantile(p_reb,probs = 0.75)-quantile(p_reb,probs = 0.25))
cd_p_pts <- 1/2*(quantile(p_pts,probs = 0.75)-quantile(p_pts,probs = 0.25))

#розмах вибірки
range_p_age <- max_p_age - min_p_age
range_p_weight <- max_p_weight - min_p_weight
range_p_height <- max_p_height - min_p_height
range_p_gp <- max_p_gp - min_p_gp
range_p_reb <- max_p_reb - min_p_reb
range_p_pts <- max_p_pts - min_p_pts

#інтервал концентрації розподілу
re_p_age <- c(mean_p_age - 3*sd_p_age,mean_p_age + 3*sd_p_age)
re_p_weight <- c(mean_p_weight - 3*sd_p_weight,mean_p_weight + 3*sd_p_weight)
re_p_height <- c(mean_p_height - 3*sd_p_height,mean_p_height + 3*sd_p_height)
re_p_gp <- c(mean_p_gp - 3*sd_p_gp,mean_p_gp + 3*sd_p_gp)
re_p_reb <- c(mean_p_reb - 3*sd_p_reb,mean_p_reb + 3*sd_p_reb)
re_p_pts <- c(mean_p_pts - 3*sd_p_pts,mean_p_pts + 3*sd_p_pts)


print_out_disp <- function(x,y,z,n,b,t){
  cat("Дисперсія: ",x,"\n")
  cat("Стандартне відхилення: ",y,"\n")
  cat("Коефіцієнт варіації: ",z,"\n")
  cat("Ймовірнесне відхилення: ", n, "\n")
  cat("Розмах вибірки: ", b, "\n")
  cat("Інтервал концентрації: ", t, "\n")
}

print_out_disp(var_p_height,sd_p_height,cv_p_height,cd_p_height,range_p_height,re_p_height)
print_out_disp(var_p_weight,sd_p_weight,cv_p_weight,cd_p_weight,range_p_weight,re_p_weight)
print_out_disp(var_p_age,sd_p_age,cv_p_age,cd_p_age,range_p_age,re_p_age)
print_out_disp(var_p_gp,sd_p_gp,cv_p_gp,cd_p_gp,range_p_gp,re_p_gp)
print_out_disp(var_p_reb,sd_p_reb,cv_p_reb,cd_p_reb,range_p_reb,re_p_reb)
print_out_disp(var_p_pts,sd_p_pts,cv_p_pts,cd_p_pts,range_p_pts,re_p_pts)



#аналіз скошеності та гостроверхності розподілу

#коефіцієнт асиметрії
skew <- function(x){
  n = length(x)
  m = mean(x)
  sum(((x-m)/sd(x))^3)/n
}
skew_p_age <- skew(p_age)
skew_p_weight <- skew(p_weight)
skew_p_height <- skew(p_height)
skew_p_gp <- skew(p_gp)
skew_p_reb <- skew(p_reb)
skew_p_pts <- skew(p_pts)

#коефіцієнт ексцесу
exc <- function(x){
  n = length(x)
  m = mean(x)
  sum(((x-m)/sd(x))^4)/n - 3
}
exc_p_age <- exc(p_age)
exc_p_weight <- exc(p_weight)
exc_p_height <- exc(p_height)
exc_p_gp <- exc(p_gp)
exc_p_reb <- exc(p_reb)
exc_p_pts <- exc(p_pts)

print_out_se <- function(x,y){
  cat("Коефіцієнт асиметрії: ",x,"\n")
  cat("Коефіцієнт ексцесу: ",y,"\n")

}

print_out_se(skew_p_height,exc_p_height)
print_out_se(skew_p_weight,exc_p_weight)
print_out_se(skew_p_age,exc_p_age)
print_out_se(skew_p_gp,exc_p_gp)
print_out_se(skew_p_reb,exc_p_reb)
print_out_se(skew_p_pts,exc_p_pts)


#стебло-листок
stem.leaf(p_age,unit = 1,m = 10)
stem.leaf(p_gp,unit = 2,m = 4)

plot(as.numeric(data$reb),as.numeric(data$player_height),main ="Scatterplot",
     col = "red",xlab="reb",ylab = "player_height",pch=19)
plot(as.numeric(data$player_weight),as.numeric(data$player_height),main ="Scatterplot",
     col = "red",xlab="player_weight",ylab = "player_height",pch=19)
plot(as.numeric(data$reb),as.numeric(data$pts),main ="Scatterplot",
     col = "red",xlab="reb",ylab = "pts",pch=19)




