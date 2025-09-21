# Librerias
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("reshape2")
#install.packages("knitr")
#install.packages("dplyr")
#install.packages("MVN")
library(ggplot2)
library(ggpubr)
library(reshape2)
library(knitr)
library(dplyr)
library(MVN)
library(MASS)
#-----------------------------------------------------------------------------------
#Paso 1. Datos para la prueba
input <- ("especie pata abdomen organo_sexual
a 191 131 53
a 185 134 50
a 200 137 52
a 173 127 50
a 171 128 49
a 160 118 47
a 188 134 54
a 186 129 51
a 174 131 52
a 163 115 47
b 186 107 49
b 211 122 49
b 201 144 47
b 242 131 54
b 184 108 43
b 211 118 51
b 217 122 49
b 223 127 51
b 208 125 50
b 199 124 46 ")
datos <- read.table(textConnection(input), header = TRUE)
datos$especie <- as.factor(datos$especie)

#  # Paso 2. Histograma de datos por clase
p1 <- ggplot(data = datos, aes(x = pata, fill = especie)) + geom_histogram(position = "identity",
alpha = 0.5,bins=30)
p2 <- ggplot(data = datos, aes(x = abdomen, fill = especie)) + geom_histogram(position =
"identity", alpha = 0.5,bins=30)
p3 <- ggplot(data = datos, aes(x = organo_sexual, fill = especie)) + geom_histogram(position =
"identity", alpha = 0.5,bins=30)
ggarrange(p1, p2, p3, nrow = 3, common.legend = TRUE)
#d<-as.numeric(datos)
#-----------------------------------------------------------------------------------
# Paso 3. Matriz de correlacion
plot(datos)
#Código 3, para la construcción de la matriz de correlación
#-----------------------------------------------------------------------------------
# Paso 3.
# Representación mediante Histograma de cada variable para cada especie
#par(mfcol = c(2, 3))
par(mar = c(1, 1, 1, 1))
for (k in 2:4) {
  j0 <- names(datos)[k]
  #br0 <- seq(min(datos[, k]), max(datos[, k]), le = 11)
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$especie)[i]
    x <- datos[datos$especie == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("especie", i0), xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "blue", lwd = 2)
  }
}

#Paso 4.
# Contraste de normalidad Shapiro-Wilk para cada variable en cada especie

datos_tidy <- melt(datos, value.name = "valor")
kable(datos_tidy %>% group_by(especie, variable) %>% summarise(p_value_Shapiro.test =
                                                                 shapiro.test(valor)$p.value))
# Paso 5

outliers <- mvn(data = datos[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")
royston_test <- mvn(data = datos[,-1], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality
hz_test <- mvn(data = datos[,-1], mvnTest = "hz")
hz_test$multivariateNormality

#-----------------------------------------------------------------------------------
# Paso 6
modelo_lda <- lda(formula = especie ~ pata + abdomen + organo_sexual,
                  data = datos)
#-----------------------------------------------------------------------------------
# Paso 7
nueva_observacion <- data.frame(pata = 194, abdomen = 124,
                                organo_sexual = 49)
predict(object = modelo_lda, newdata = nueva_observacion)

