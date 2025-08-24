#-------------------------------------------------------
#         MAQUINAS DE SOPORTE VECTORIAL (SVM)
#-------------------------------------------------------
library(tidyverse)
library(caret)
library(e1071)
library(readr)

vector <- read_csv("10 Mineria de Datos/Arboles_MaquinaSoporte/vector.csv")
View(vector)

#Gráfico de dispersión para verlos datos en clases(grupos)
#no se observa que esten separados por eso aplicaremos los vectores de soporte
ggplot(vector, aes(x= x1, y= x2, color = as.factor(clase)))+
  geom_point(size = 5)


#dividir la data en muestra de entrenamiento y de prueba
set.seed(12345)
muestra = createDataPartition(vector$clase,p=0.8, list = F)
train = vector[muestra,]
test= vector[-muestra,]

#se convierte en factores
train$clase = as.factor(train$clase)
test$clase = as.factor(test$clase)
str(train$clase)

#crear modelo

svm_c = svm(clase~ x1+ x2, data =train,
            kernel =  "linear",
            scale =T, cost =10)
summary(svm_c)

#se gráfica la máquina de soporte
plot(svm_c, data =train,x1 ~ x2)


#----- Kernel Lineal ----
#un mejor modelo con opción tune
# se da distintos valores al costo y se escoje el que minimize
svm_l = tune(svm,clase~ x1+ x2, data =train,
             kernel =  "linear", scale =T,
             ranges =list(cost =c(0.01, 0.1, 1, 2, 5, 10)))

names(svm_l)

#mejores parámetros c=0.01
svm_l$best.parameters
#mejores performance
svm_l$performances

ggplot(data = svm_l$performances, aes(x =cost, y = error))+
  geom_line()+
  geom_point()+
  theme_bw()+
  theme(legend.position = "bottom")

#Analizamos el mejos modelo 
svm_lb = svm_l$best.model

#evaluar el modelo
prediccion_l = predict(svm_lb, test)

confusionMatrix(test$clase, prediccion_l)
#accuracy = 75%

#-------- kernel polinomial---

svm_p = tune(svm,clase~ x1+ x2, data =train,
             kernel =  "polynomial", scale =T,
             ranges =list(cost =c(0.01, 0.1, 1, 2, 5, 10),
                          degree =c(2,3,4)))
svm_p$performances

ggplot(data = svm_p$performances, aes(x =cost, y = error, color = as.factor(degree)))+
  geom_line()+
  geom_point()+
  theme_bw()+
  theme(legend.position = "bottom")

svm_pb = svm_p$best.model

#evaluar el modelo
prediccion_p = predict(svm_pb, test)

confusionMatrix(test$clase, prediccion_p)
#accuracy = 77.5% (un mejora respecto al kernel lineal)


#-------- kernel Gausssiano (radial---

svm_g = tune(svm,clase~ x1+ x2, data =train,
             kernel =  "radial", scale =T,
             ranges =list(cost =c(0.01, 0.1, 1, 2, 5, 10),
                          gamma =c(0.1, 0.5,1,5,10)))
svm_g$performances

ggplot(data = svm_g$performances, aes(x =cost, y = error, color = as.factor(gamma)))+
  geom_line()+
  geom_point()+
  theme_bw()+
  theme(legend.position = "bottom")

svm_gb = svm_g$best.model
svm_gb
#evaluar el modelo
prediccion_g = predict(svm_gb, test)

confusionMatrix(test$clase, prediccion_g)
#accuracy = 82.5% (un mejora respecto al kernel lineal, polinomico)


