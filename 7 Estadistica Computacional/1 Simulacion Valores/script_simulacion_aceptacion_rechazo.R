### Simular valores de Variable Aleatoria por el método de aceptación-rechazo

## Generando la función para simular los valores de la variable aleatoria por el método
## de aceptación-rechazo

metodo_ar <- function(nsims){
  muestra <- numeric(nsims)
  aceptado <- 0
  intento <- 0
  while (aceptado < nsims) {
    x <- runif(1, 0, 1) # Valor de la función propuesta g(x), es una Uniforme [0,1]
    u <- runif(1, 0, 1) # Valor de la uniforme para comparar 
    
    if (u <= x) { #Criterio de aceptación (f(x)/c*g(x)) = 2x/2  = x
      aceptado <- aceptado + 1
      muestra[aceptado] <- x
    }
    intento <- intento + 1
  }
  
  return(muestra)
}

## Generar 100 valores
set.seed(12005)
muestra <- metodo_ar(100)
muestra

## Histograma de los valores simulados
hist(muestra, probability = TRUE, breaks = 25, 
     xlab = "Muestra con valores simulados de X",
     ylab = "Densidad de X",
     xlim = c(0,1), ylim = c(0, 3.5),
     col = "green")
curve(2*x, from = 0, to = 1, lwd=3, col = "blue", add = TRUE)
legend("topright",legend = c("Función de densidad: 2x","Histograma"), col = c("blue","green"), lwd =3)

## Interpreten el histograma para 5000 valores y después generar
## Una muestra con 5000 valores

## Generar 5000 valores
set.seed(12005)
muestra <- metodo_ar(5000)
muestra

## Histograma de los valores simulados
hist(muestra, probability = TRUE, breaks = 25, 
     xlab = "Muestra con valores simulados de X",
     ylab = "Densidad de X",
     xlim = c(0,1), ylim = c(0, 3.5),
     col = "green")
curve(2*x, from = 0, to = 1, lwd=3, col = "blue", add = TRUE)
legend("topright",legend = c("Función de densidad: 2x","Histograma"), col = c("blue","green"), lwd =3)


### Simular valores de Variable Aleatoria por el método de aceptación-rechazo

## Generando la función para simular los valores de la variable aleatoria por el método
## de aceptación-rechazo

metodo_ar <- function(nsims){
  muestra <- numeric(nsims)
  aceptado <- 0
  intento <- 0
  while (aceptado < nsims) {
    x <- runif(1, 0, 1) # Valor de la función propuesta g(x), es una Uniforme [0,1]
    u <- runif(1, 0, 1) # Valor de la uniforme para comparar 
    
    if (u <= x^2) { #Criterio de aceptación (f(x)/c*g(x)) = 3x^2/3  = x
      aceptado <- aceptado + 1
      muestra[aceptado] <- x
    }
    intento <- intento + 1
  }
  
  return(muestra)
}

## Generar 100 valores
set.seed(12005)
muestra <- metodo_ar(100)
muestra

## Histograma de los valores simulados
hist(muestra, probability = TRUE, breaks = 25, 
     xlab = "Muestra con valores simulados de X",
     ylab = "Densidad de X",
     xlim = c(0,1), ylim = c(0, 3.5),
     col = "green")
curve(3*x^2, from = 0, to = 1, lwd=3, col = "blue", add = TRUE)
legend("topright",legend = c("Función de densidad: 2x","Histograma"), col = c("blue","green"), lwd =3)

## Interpreten el histograma para 5000 valores y después generar
## Una muestra con 5000 valores

## Generar 5000 valores
set.seed(12005)
muestra <- metodo_ar(1000000)
muestra

## Histograma de los valores simulados
hist(muestra, probability = TRUE, breaks = 100, 
     xlab = "Muestra con valores simulados de X",
     ylab = "Densidad de X",
     xlim = c(0,1), ylim = c(0, 3.5),
     col = "green")
curve(3*x^2, from = 0, to = 1, lwd=3, col = "blue", add = TRUE)
legend("topright",legend = c("Función de densidad: 3x^2","Histograma"), col = c("blue","green"), lwd =3)
boxplot(muestra, col = "skyblue")




### Simular valores de Variable Aleatoria por el método de aceptación-rechazo

# 1. Generar dos numeros aleatorios U_1, U_2.
set.seed(3)
u1 <- runif(1)
u2 <- runif(1)

# 2. Comparar con f(x)/cg(x)
while(u2>(256/27)*u1*(1-u1)^3){
  u1 <- runif(1)
  u2 <- runif(1)
}
X=u1
X


ddensidad<- function(x){
  20*x*(1-x)^3
}
## Creamos una funcion para simular los valores de la variable
beta20 <- function(){
  # 1. Generar dos numeros aleatorios U_1, U_2.
  u1 <- runif(1)
  u2 <- runif(1)
  # 2. Comparar con f(x)/cg(x)
  while(u2 > (256 / 27) * u1 * (1 - u1) ^ 3){
    u1 <- runif(1)
    u2 <- runif(1)
  }
  u1
}
library(dplyr)
library(plyr)
sims <- rdply(10000, beta20)
sims


hist(sims$V1, breaks="FD", freq=FALSE,
     xlim = c(0,1),ylim = c(0,2.5),main = "Histograma para valores simulados")
curve(ddensidad(x), add=TRUE, col="red")
