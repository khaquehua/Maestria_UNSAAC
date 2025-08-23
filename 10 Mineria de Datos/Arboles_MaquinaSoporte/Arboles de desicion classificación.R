#-------------------------------------------------------
#               ARBOLES DE DECISION 
#-------------------------------------------------------
install.packages("rpart.plot")
install.packages("rpart")
install.packages("caret")

library(tidyverse)
library(rpart.plot)
library(rpart)
library(caret)

library(haven)
lowbirth <- read_dta("D:/Dictado de clases/UNSAAC/Maestría/Minería de Datos 2025/Clase6/lowbirth.dta")
str(lowbirth)

lowbirth$low = factor(lowbirth$low)

# dividir la data en entrenamiento y testeo
set.seed(12345)
muestra = createDataPartition(lowbirth$low, p = 0.7 , list =F)

train = lowbirth[muestra,]
test = lowbirth[-muestra,]

#---crear nuestro arbol
arbol = rpart(low ~  age + lwt + smoke+ ht + ui +race,
              data =train, method = "class")

arbol

rpart.plot(arbol)

library(rattle)
fancyRpartPlot(arbol)

# evaluar el modelo
predict_arbol = predict(arbol, test, type ="class")
predict_arbol

#matriz de confusion
confusion = table(test$low, predict_arbol, dnn = c("real", "Estimada"))
confusion

PCC = sum(diag(confusion))/sum(confusion)
PCC

# libreria de caret
confusionMatrix(test$low, predict_arbol)
#de cada 100 bebes nacidos el modelo clasifica correctamente a 64 de ellos

