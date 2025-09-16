install.packages("MASS", dependencies = T)
# Librerias
install.packages("ggplot2")#
install.packages("ggpubr")
install.packages("reshape2")
install.packages("knitr")
install.packages("dplyr")
install.packages("MVN")
library(ggplot2)
library(ggpubr)
library(reshape2)
library(knitr)
library(dplyr)
library(MVN)
library(MASS)
library(MASS)

iris<-iris
iris$Species<-factor(rep(c("s","c","v"),c(50,50,50)))
#matriz correlación
plot(iris)
iris.lda<-lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris)
names(iris.lda)
plot(iris.lda)
#entrenamiento
train<-sample(1:150,(40*100)/150)
entrena<-iris[train,]
prueba<-iris[-train,]
iris2.lda<-lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=entrena)
names(iris2.lda)
plot(iris2.lda)
iris.lda
iris2.lda
iris.pred<-predict(iris2.lda,prueba)
iris.pred$class
table(prueba$Species, iris.pred$class)
iris.pred1<-predict(iris.lda,prueba)
iris.pred1$class
table(prueba$Species, iris.pred1$class)