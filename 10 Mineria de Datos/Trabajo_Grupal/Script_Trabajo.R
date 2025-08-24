#-------------------------------------------------------
#               ARBOLES DE DECISION - REGRESION
#-------------------------------------------------------

library(tidyverse)
library(rpart.plot)
library(rpart)
library(caret)
library(MASS)

#Precio medio de la vivienda (medv) en función de las variables independientes
?Boston
data(Boston)
Data = Boston
str(Data)

#crear muestra de entrenamiento y testeo
set.seed(12345)
muestra = createDataPartition(Data$medv, p = 0.75, list = F)
train = Data[muestra,]
test = Data[-muestra,]

#crear el arbol
arbol_r = rpart(medv ~ ., data = train)
arbol_r

#graficamos el arbol de regresión
rpart.plot(arbol_r)


#evaluacion del modelo
prediccion_r = predict(arbol_r, test)
table(prediccion_r)

#ECM
ECM_arbol = mean((test$medv- prediccion_r)^2)
ECM_arbol

