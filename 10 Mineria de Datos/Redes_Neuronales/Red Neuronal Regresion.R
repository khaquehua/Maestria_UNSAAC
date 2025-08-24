#-------------------------------------------------------
#               Redes neuronales
#------------------- REGRESION-------------------------
rm(list = ls())
#install.packages("neuralnet")
library(neuralnet)
library(tidyverse)
library(caret)
library(MASS)
#leer la data
?Boston

#red neuronal regresion
data <- Boston
apply(data,2,function(x) sum(is.na(x)))
sapply(data, function(x) sum(!is.finite(x)))
sum(duplicated(data))
summary(data)

muestra = createDataPartition(data$crim, p = 0.7, list =F)
data_s =scale(data) #estandarización
train_s = data_s[muestra,]
test_s = data_s[-muestra,]

#problema de regresion: linear.output = TRUE
#problema de classificacion: linear.output = FALSE

nn = neuralnet(medv~.,
               data=train_s,
               hidden=c(5,3),
               linear.output=TRUE)

plot(nn)

#prediccion
pred = neuralnet::compute(nn, test_s)
pred$neurons #calculo de los valores en neuronas

# tamaños por capa
lapply(pred$neurons, dim)

# primera capa (input, con bias)
head(pred$neurons[[1]])

# primera capa oculta (activaciones + bias)
head(pred$neurons[[2]])   # valores ~ (0,1)

# segunda capa oculta
head(pred$neurons[[3]])

pred$net.result #predicicon final

pred_niveles = pred$net.result*sd(data$medv) + mean(data$medv)
head(pred_niveles)
head(test_s[,14])

#Metricas 
y_true <- data$medv[-muestra]              # y real (escala original)
y_hat  <- as.numeric(pred_niveles)         # predicciones (escala original)

ECM  <- mean((y_hat - y_true)^2)
RMSE <- sqrt(ECM)
MAE  <- mean(abs(y_hat - y_true))

mu_train <- mean(data$medv[muestra])       # media del train 
R2  <- 1 - sum((y_hat - y_true)^2) / sum((y_true - mu_train)^2)
RMSE_base <- sqrt(mean((y_true - mu_train)^2))
mejora_rel <- 1 - RMSE/RMSE_base

c(ECM=ECM, RMSE=RMSE, MAE=MAE, R2=R2, RMSE_base=RMSE_base, Mejora=mejora_rel)

#Calibración (observado vs. predicho)
plot(y_true, y_hat, pch=19, col="#1f77b455",
     xlab="Real (medv)", ylab="Predicho"); abline(0,1,lty=2,col="red")
