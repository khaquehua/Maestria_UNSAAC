## Simulación de valores de variable aleatoria por el método de la composición
set.seed(123)

n <- 100
# Probabilidades de mezcla
p1 <- 0.6
p2 <- 0.4

# paso 1: Generar la variable auxiliar para identificar a qué distribución pertenece
I <- sample(c(1,2), size = n, replace = TRUE, prob = c(p1,p2))
I

# paso 2: Crear un vector vacío llamado muestra
muestra <- numeric(n)
muestra

# paso 3: Simular los valores para la nueva distribución
for (i in 1:n) {
  if (I[i] == 1){
    muestra[i] <- rnorm(1, mean = 0, sd = 1)
  } else { ## I[i] == 2
    muestra[i] <- rnorm(1, mean = 3, sd = 2)
  }
}

muestra

## Histograma de los valores simulados
hist(muestra, breaks = 20, probability = TRUE)
summary(muestra)
boxplot(muestra, col = "skyblue")
