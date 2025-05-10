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
nsims <- 10
u <- runif(nsims, 0, 1)
u
# Simulando los nuevos valores de x en función de u
lambda <- 2
x <- -(1/lambda)*log(1-u)
x

# Generando histograma para los nuevos valores de x
hist(x)
hist(u)

plot(x,u)

