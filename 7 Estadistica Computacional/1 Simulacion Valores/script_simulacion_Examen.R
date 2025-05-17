## Simulación de n valores variable aleatoria que siguen la distribución dada
set.seed(123)
nsims <- 10000
u <- runif(nsims, 0, 1)
u
# Simulando los nuevos valores de x en función de u
x <- (u)^(1/3)
x
# resumen de medidas descriptivas de la variable simulada
summary(x)
# Generando histograma para los nuevos valores de x
hist(x, probability = TRUE, breaks = 100, 
     xlab = "Muestra con valores simulados de X",
     ylab = "Densidad de X",
     col = "green")
curve(3*x^2, from = 0, to = 1, lwd=3, col = "blue", add = TRUE)
legend("topleft",legend = c("Función de densidad: 3x^2","Histograma"), 
       col = c("blue","green"), lwd =3)
boxplot(x, col = "skyblue", main = "Diagrama de caja para los valores simulados") 

