## Pruebas para verificar normalidad multivariante
library(MVN)
library(readxl)
datos <- read_excel("9 Analisis Multivariado/Inferencia multivariante/PesoTalla.xlsx")
datos <- datos[,-1]
View(datos)

# datos del ejercicio
mu0 = matrix(c(70,170),2,1) ; mu0
var = matrix(c(20,100,100,1000),2,2) ; var
n=length(datos$X1) ; n
p=dim(datos)[2]
p

# datos de la muestra
xbar=colMeans(datos)
xbar

# estadistico de prueba
chic= n*(t(xbar-mu0))%*%solve(var)%*%(xbar-mu0)
chic

qchisq(0.95, df = 2)

# Datos del ejercicio 2

mu0 <- 













