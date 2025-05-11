(-1/2) * log(1 - 0.23)

#Simulación de valores de variable aleatoria que sigua una distribución exponencial
?runif
u <- runif(1, 0, 1)
u

set.seed(123)
u <- runif(1, 0, 1)
u

set.seed(15)
u <- runif(1, 0, 1)
u

set.seed(12345)
u <- runif(1, 0, 1)
u

#Simularemos un valor de x en función de u
lambda <- 2
x <- -(1/lambda)*log(1-u)
x

#set.seed(12345)
u <- runif(1, 0, 1)
u

#Simularemos un valor de x en función de u
lambda <- 2
x <- -(1/lambda)*log(1-u)
x



## Simulación de n valores variable aleatoria que siguen una distribución exponencial
set.seed(12345)
nsims <- 10000
u <- runif(nsims, 0, 1)
u
# Simulando los nuevos valores de x en función de u
lambda <- 2
x <- -(1/lambda)*log(1-u)
x

# Generando histograma para los nuevos valores de x
hist(x, breaks = "FD", freq = FALSE, xlab = "Valores simulados de X",
     ylab = "Densidad de los valores simulados de X", col = "red",
     main = "Histograma de la V.A. X")
curve(dexp(x, lambda), lwd = 3, add = TRUE)

boxplot(x, col = "skyblue")

# resumen de medidas descriptivas de la variable simulada
summary(x)

hist(u)

plot(x,u)

## Simulación de n valores variable aleatoria que siguen una distribución uniforme
set.seed(12345)
nsims <- 10000
u <- runif(nsims, 0, 1)
u
# Simulando los nuevos valores de x en función de u
b <- 3
a <- 2
x <- u*(b-a)+a
x

# Generando histograma para los nuevos valores de x
hist(x, breaks = "FD", freq = FALSE, xlab = "Valores simulados de X",
     ylab = "Densidad de los valores simulados de X", col = "red",
     main = "Histograma de la V.A. X")
curve(dunif(x, a, b), lwd = 3, add = TRUE)

boxplot(x, col = "skyblue")

# resumen de medidas descriptivas de la variable simulada
summary(x)

hist(u)

plot(x,u)


## Simulación de n valores variable aleatoria que siguen una distribución uniforme
set.seed(12345)
a <- 2
b <- 4
nsims <- 100
#Primer caso generar los casos de la uniforme
u <- runif(nsims, 0, 1)
u

# simulando los valores de x que sigue una distribución uniforme
x <- u * (b-a)+a
x

# Generando histograma para los nuevos valores de x
hist(x, breaks = "FD", freq = FALSE, xlab = "Valores simulados de X",
     ylab = "Densidad de los valores simulados de X", col = "red",
     main = "Histograma de la V.A. X")
curve(dunif(x, a, b), lwd = 3, add = TRUE)

boxplot(x, col = "skyblue")

# resumen de medidas descriptivas de la variable simulada
summary(x)

hist(u)

plot(x,u)

## Simulación de n valores variable aleatoria que siguen una distribución pareto
#library(distrEx)
set.seed(12345)
alpha <- 3
x_m <- 1
nsims <- 1000
#Primer caso generar los casos de la uniforme
u <- runif(nsims, 0, 1)
u

# simulando los valores de x que sigue una distribución uniforme
x <- x_m / ((1-u)^(1/alpha))
x

# Generando histograma para los nuevos valores de x
hist(x, breaks = "FD", freq = FALSE, xlab = "Valores simulados de X",
     ylab = "Densidad de los valores simulados de X", col = "red",
     main = "Histograma de la V.A. X")
curve(dpareto(x, alpha, x_m), lwd = 3, add = TRUE)

boxplot(x, col = "skyblue")

# resumen de medidas descriptivas de la variable simulada
summary(x)

hist(u)

plot(x,u)
