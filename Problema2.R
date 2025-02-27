#Codigo para problema 2

mis_dades <- iris
dim(mis_dades)
names(mis_dades)
mis_dades$Petal.Length
mean(mis_dades$Petal.Length)
sd(mis_dades$Petal.Length)
hist(mis_dades$Petal.Length)

x<-mis_dades$Petal.Length
y<-mis_dades$Sepal.Length
plot(x,y)

#pregunta 1 (pendent (m)):

m<-sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)

#consola respon 0.4089223, m = 0.4089223
m

#pregunta 2, recta de regressio
b <- mean(y)-m*mean(x)
#b és la recta de regressio


#pregunta 3, predicció amb valor 1.5 donat
m*1.5+b


#tot resolt, pero ara ho resol d'una manera mes senzilla

mod<-lm(y~x)
summary(mod)

ypredict <- predict(mod, data.frame(x=x))

plot(x,y)
lines(x, ypredict)

#coeficient de determinacio
Rsq <- sum((ypredict-mean(y))^2)/sum((y-mean(y))^2)
Rsq
summary(mod)
