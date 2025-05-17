## Simulación de valores de variable aleatoria por el método de la composición
# f(x) = 0.3 N(2,1) + 0.5 Exp (3.5) + 0.2 Exp (1.5)
# Kevin Haquehua Apaza
set.seed(123)
n <- 1000
# Probabilidades de mezcla
p1 <- 0.3
p2 <- 0.5
p3 <- 0.2

# paso 1: Generar la variable auxiliar para identificar a qué distribución pertenece
I <- sample(c(1,2,3), size = n, replace = TRUE, prob = c(p1,p2,p3))
I

# paso 2: Crear un vector vacío llamado muestra
muestra <- numeric(n)
muestra

# paso 3: Simular los valores para la nueva distribución
for (i in 1:n) {
  if (I[i] == 1){
    muestra[i] <- rnorm(1, mean = 2, sd = 1)
  } else { 
    if (I[i] == 2) { ## I[i] == 2
      muestra[i] <- rexp(1, rate = 3.5)
    } else { ## I[i] == 3
      muestra[i] <- rexp(1, rate = 1.5)
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
