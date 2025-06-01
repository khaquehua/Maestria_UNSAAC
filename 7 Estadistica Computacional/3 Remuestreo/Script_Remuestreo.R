## SCRIPT REMUESTREO

# Muestra original obtenida de una población
datos <- c(5.2, 4.8, 6.1, 5.9, 4.5, 6.3, 5.7, 5.0, 6.0, 5.4)
# Numero de remuestreos
B <- 1000
# Guardamos las medias bootstrap en un vector
medias_bootstrap <- numeric(B)
medias_bootstrap
set.seed(123)

for (i in 1:B) {
  muestra_bootstrap <- sample(datos, size = length(datos), replace = TRUE)
  medias_bootstrap[i] <- mean(muestra_bootstrap)
}

# Muestra el vector conteniendo las 1000 medias remuestreadas
medias_bootstrap
mean(datos)

# Intervalo de confianza del 95%
ic_95 <- quantile(medias_bootstrap, probs = c(0.025, 0.975))
print(ic_95)

#histograma de las medias muestreadas
hist(medias_bootstrap, breaks=30, 
     main="Distribución Bootstrap de la media",
     xlab="media", col="lightblue", border="white")
abline(v=ic_95, col="red", lty=2)
