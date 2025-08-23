#-------------------------------------------------------
#               K-Vecinos mas Cercanos
#-------------------------------------------------------

library(readr)
diabetes <- read_csv("10 Mineria de Datos/NaiveBayes_KNN/diabetes.csv")
str(diabetes)
View(diabetes)

#install.packages(kknn)
library(tidyverse)
library(kknn)
library(ggplot2)
library(caret)

#Descriptivos de las variables independientes
summary(diabetes)

#Indica los tipos de diabetes
table(diabetes$class)

str(diabetes)
# escalar la data (para tener las medidas en la misma unidad)
diabetes_scale = scale(diabetes[,-1], center = T, scale = T)
diabetes_scale = data.frame(diabetes_scale,class= diabetes$class)

#se convierte las clasesen factores
diabetes_scale$class = as.factor(diabetes_scale$class)

#segmentar la data en test y train
set.seed(1234)
muestra = createDataPartition(diabetes_scale$class, p =0.8, list = F)

train_k = diabetes_scale[muestra,]
test_k = diabetes_scale[-muestra,]

#graficar la relacion entre las variables

p1 <- ggplot(train_k, aes(glucose, insulin, col = class))+
  geom_point(aes(shape= class), size = 3)+
  theme_bw()

p2 <- ggplot(train_k, aes(glucose, sspg, col = class))+
  geom_point(aes(shape= class), size = 3)+
  theme_bw()

p3<- ggplot(train_k, aes(sspg, insulin, col = class))+
  geom_point(aes(shape= class), size = 3)+
  theme_bw()

p1+p2+p3

#estimar el modelo KNN con 4 vecinos m?s cercano y el kernel me ayuda a dar pesos

modelo_vecino = kknn(class ~ glucose +insulin+sspg,
                     train_k, test_k, k =4, kernel = "gaussian")

#predicci?n que se ha realizado a la muestra de prueba
modelo_vecino$fitted.values

#matriz de clase mas cercano para los 4 vecinos
modelo_vecino$CL
#matriz de pesos que fueron asignados a cada vecino
modelo_vecino$W

#Sknn vecino mas cercano
library(klaR)
partimat(class ~ glucose +insulin+sspg,
         data= train_k, method = "sknn",
         image.colors = c("pink", "green","skyblue"), nplots.vert = 2)
table(diabetes$class)

#MAtriz de confusion para k = 4 vecinos
pred_vecino = fitted(modelo_vecino)
MC = table(pred_vecino, test_k$class)
MC
PCC = sum(diag(MC))/sum(MC)
PCC
confusionMatrix(pred_vecino, test_k$class)

#numero optimo de vecinos
vecino_opt = train.kknn(class ~.,
                        data = train_k, kmax = 10, kernel = "gaussian")
summary(vecino_opt)

