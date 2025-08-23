#-------------------------------------------------------
#              Naive Bayes
#-------------------------------------------------------
#install.packages("naivebayes")
library(e1071)      # Métodos de ML, contiene Naive Bayes clásico
library(caret)      # Funciones para particionar, entrenar y evaluar modelos
library(tidyverse)  # Conjunto de librerías (ggplot2, dplyr, etc.)
library(ggpubr)     # Gráficos estadísticos
library(readxl)     # Leer archivos Excel
library(tidytext)   # Procesamiento de texto
library(naivebayes) # Implementación de Naive Bayes optimizada
library(tm)         # Text mining (para preprocesamiento de texto)

# Se lee la base de datos
ep <- read_excel("D:/Dictado de clases/UNSAAC/Maestría/Minería de Datos 2025/Clase6/electronics-purchase.xlsx")
View(ep)
table(ep$Purchase) #

#particionamos la muestra en entrenamiento y prueba
set.seed(12345)  #Fija la semilla para que los resultados sean reproducibles.
muestra = createDataPartition(ep$Purchase, p =0.70, list =F) #Crea un muestra, manteniendo la proporción de clases de los datos.
train = ep[muestra,]
test = ep[-muestra,]


#modelo
modelo <- naive_bayes(formula = Purchase ~ .,  data = train) #predice purchase usando las demás variables
modelo

#predicciones
#prediccion <- predict(train, test)
#prediccion: newdata Aplica el modelo sobre los datos de prueba.
#prediccion: Vectore de clases predichas (ejemplo: "Yes", "No")
prediccion <- predict(modelo, newdata = test) 
prediccion
#Matriz de confusion
#table() Crea una tabla cruzada entre:
#prediccion → lo que el modelo predijo.
#test$Purchase → la clase real en los datos de prueba.
#MC: Es la matriz de confusión, que muestra: Verdaderos positivos (TP)
#Verdaderos negativos (TN), Falsos positivos (FP) y Falsos negativos (FN)

MC = table(prediccion, test$Purchase)
MC
