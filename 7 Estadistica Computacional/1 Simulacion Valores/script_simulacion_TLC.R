## Simulación de valores por el metodo de TLC
# Establecer el número de variables uniformes por muestra
n <- 10 # generando 10 V.A. que seguiran distribución uniforme
nsims <- 1000 # Número total de simulaciones

# Simulación
set.seed(123)
uniformes <- matrix(runif(nsims * n, min = 0, max = 1), nrow = nsims)
uniformes
head(uniformes)
tail(uniformes)
suma_uniformes <- rowSums(uniformes)
suma_uniformes

Z1 <- (suma_uniformes - n * 0.5)/(sqrt(1/12))* sqrt(n) ## Sigue N (0,1)
Z1

Z2 <- (suma_uniformes - n * 0.5)/(sqrt(1/12)* sqrt(n)) ## Sigue N (0,1)
Z2

Z <- (suma_uniformes - n * 0.5)/(sqrt(1/12)* sqrt(n)) ## Sigue N (0,1)
Z


ver <- data.frame(Z1 = Z1, Z2 = Z2)

# Histograma
hist(Z, breaks = 50, probability = TRUE, col = "skyblue",
     main = "Simulación de una normal estándar usando el TLC",
     xlab = "Valores simulados")
# Curva de densidad teórica normal
curve(dnorm(x, mean = 0, sd = 1), from = -40, to = 40, add = TRUE,
      col = "red")
# Generar boxplot
boxplot(Z)
