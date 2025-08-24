#------------------------------------------------------------
#           SCRIPT REGRESION - data (Administrador)
#------------------------------------------------------------
# Leer base de datos
library(readxl)
Administrador <- read_excel("10 Mineria de Datos/Trabajo_Grupal/Administrador.xlsx")
Administrador <- Administrador[,-2]
colnames(Administrador)[7] <- c("Tarjetas")

# Analisis exploratorio de datos
# Diagrama de cajas de variables predictora (Ingresos)
boxplot(Administrador$Ingresos, col = "yellow",
        main = "Diagrama de cajas ingresos",
        ylab = "Ingresos (S/.)")
summary(Administrador$Ingresos)
# En el diagrama de cajas se puede observar que la variable ingreso presenta
# una asimetría a la derecha, el ingreso mínimo es de 21240 y el ingreso máximo es de 91100
# en promedio los ingresos son de 48646.


# Diagrama de dispersión con respecto a las otras variables
pairs(Administrador, psch =19)
cor(Administrador)
# En el gráfico de correlación se puede onservar que las variables edad y residencia presenta
# una correlación alta, asimismo, las variables  tiempo de empleo con el tiempo de residencia y tiempo de empleo con edad 
# presentan una alta correlación. Esto quiere decir que: a medida que el empleador tenga más edad, el tiempo que vive en su recidencia actual 
# y el tiempo trabajando para el empleador actual será mayor. Tambien se puede observar que a mayor tiempo de trabajo para el empleador actual
# mayor será el tiempo de su residencia actual.

# Primero estandarizemos
data <- data.frame(scale(Administrador))

#-------------------------------------------------------
# Data Bootstrap
n <- nrow(Administrador)   # número de observaciones originales (30)
B <- 500          # tamaño deseado de la nueva muestra

# Bootstrap: re-muestreo con reemplazo
data <- data[sample(1:n, size = B, replace = TRUE), ]

# Veamos la distribucion de los nuevos datos
boxplot(data$Ingresos, col = "yellow",
        main = "Diagrama de cajas ingresos",
        ylab = "Ingresos (S/.)")
summary(data$Ingresos)

# Diagrama de dispersión con respecto a las otras variables
pairs(data, psch =19)
cor(data)

#-------------------------------------------------------
#               ARBOLES DE DECISION - REGRESION
#-------------------------------------------------------

library(tidyverse)
library(rpart.plot)
library(rpart)
library(caret)
library(MASS)

#crear muestra de entrenamiento y testeo
set.seed(12345)
muestra = createDataPartition(data$Ingresos, p = 0.75, list = F)
train = data[muestra,]
test = data[-muestra,]

#crear el arbol
arbol_r = rpart(Ingresos ~ ., data = train)
arbol_r

#graficamos el arbol de regresión
rpart.plot(arbol_r)

#evaluacion del modelo
prediccion_r = predict(arbol_r, test)
table(prediccion_r)

#ECM
ECM_arbol = mean((test$Ingresos- prediccion_r)^2)
ECM_arbol

#-------------------------------------------------------
#               Redes neuronales
#------------------- REGRESION-------------------------
library(neuralnet)
library(tidyverse)
library(caret)
library(MASS)

#red neuronal regresion
#problema de regresion: linear.output = TRUE
#problema de classificacion: linear.output = FALSE

nn = neuralnet(Ingresos~.,
               data=train,
               hidden=c(5,3),
               linear.output=TRUE)

plot(nn)

#prediccion
pred = neuralnet::compute(nn, test)
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

pred_niveles = pred$net.result*sd(data$Ingresos) + mean(data$Ingresos)
head(pred_niveles)
head(test[,1])

#Metricas 
y_true <- data$Ingresos[-muestra]              # y real (escala original)
y_hat  <- as.numeric(pred_niveles)         # predicciones (escala original)

ECM  <- mean((y_hat - y_true)^2)
RMSE <- sqrt(ECM)
MAE  <- mean(abs(y_hat - y_true))

mu_train <- mean(data$Ingresos[muestra])       # media del train 
R2  <- 1 - sum((y_hat - y_true)^2) / sum((y_true - mu_train)^2)
RMSE_base <- sqrt(mean((y_true - mu_train)^2))
mejora_rel <- 1 - RMSE/RMSE_base

c(ECM=ECM, RMSE=RMSE, MAE=MAE, R2=R2, RMSE_base=RMSE_base, Mejora=mejora_rel)

#Calibración (observado vs. predicho)
plot(y_true, y_hat, pch=19, col="#1f77b455",
     xlab="Real (Ingresos)", ylab="Predicho"); abline(0,1,lty=2,col="red")

