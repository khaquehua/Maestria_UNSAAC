set.seed(123)
N <- 10
n <- rep(10, N)
theta_true <- rbeta(N, 2, 5)
y <- rbinom(N, size = n, prob = theta_true)

#:::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::   MUESTREADOR DE GIBBS   ::::::::::::
#:::::::::::     DISTRIBUCION BETA    ::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::
#Insercion de los hiperparametros
a <- 2
b <- 5
iters <- 1000
theta_samples <- matrix(NA, nrow = iters, ncol = N)

# Inicialización
theta_actualizado <- rep(0.5, N)

for (t in 1:iters) {
  for (i in 1:N) {
    alpha_post <- y[i] + a # Es el parametro de la distribucion beta
    beta_post <- n[i] - y[i] + b # Es el parametro de la distribucion beta
    theta_actualizado[i] <- rbeta(1, alpha_post, beta_post)
  }
  theta_samples[t, ] <- theta_actualizado
}
#tail(theta_samples)
#:::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::   Seleccionar un grupo   ::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::

i <- 10 # Grupo que vamos a observar (La marginal de teta)
cadena_i <- theta_samples[, i]

#:::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::       Visualización      ::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::

# Cadena de Markov
plot(cadena_i, type = "l", col = "blue",
     xlab = "Iteración", ylab = expression(theta[1]),
     main = expression("Cadena de Markov para " * theta[1])) 

# Histograma
hist(cadena_i, breaks = 30, probability = TRUE,
     col = "lightblue", main = expression("Histograma de "* theta[1]),
     xlab = expression(theta[1]))

# Densidad posterior
lines(density(cadena_i), col = "red", lwd = 2)

