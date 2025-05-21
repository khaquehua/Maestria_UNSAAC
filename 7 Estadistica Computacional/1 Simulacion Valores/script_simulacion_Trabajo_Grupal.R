# Kevin Haquehua Apaza
set.seed(123)
n <- 1000
# Probabilidades de mezcla
p1 <- 0.4
p2 <- 0.1
p3 <- 0.4

# paso 1: Generar la variable auxiliar para identificar a qué distribución pertenece
I <- sample(c(1,2,3), size = n, replace = TRUE, prob = c(p1,p2,p3))
I

# paso 2: Crear un vector vacío llamado muestra
muestra <- numeric(n)
muestra

# paso 3: Simular los valores para la nueva distribución
for (i in 1:n) {
  if (I[i] == 1){
    muestra[i] <- rbeta(1, shape1 = 1, shape2 = 0.5)
  } else { 
    if (I[i] == 2) { ## I[i] == 2
      muestra[i] <- runif(1, min = 0, max = 1)
    } else { ## I[i] == 3
      muestra[i] <- rbeta(1, shape1 = 0.5, shape2 = 1)
    }
  }
}

muestra

## Histograma de los valores simulados
hist(muestra, breaks = 100, probability = TRUE,
     xlab = "Muestra con valores simulados de X",
     ylab = "Densidad de X",
     col = "green")
boxplot(muestra, col = "skyblue")


## Histograma de los valores simulados
hist(muestra, breaks = 100, probability = TRUE,
     xlab = "Muestra con valores simulados de X",
     ylab = "Densidad de X",
     col = "green")
summary(muestra)
boxplot(muestra, col = "skyblue")
