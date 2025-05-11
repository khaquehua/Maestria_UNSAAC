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

