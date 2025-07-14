# pregunta 1
# Información del problema
n        <- 20
Estatura <- c(69,74,68,70,72,67,66,70,76,68,72,79,74,67,66,71,74,75,75,76)
Peso     <- c(153,175,155,135,172,150,115,137,200,130,140,265,185,112,140,150,165,185,210,220)
# Datos
data <- cbind(Estatura,Peso)
# Matriz de covarianza
Sigma <- matrix(c(20,100,100,1000),ncol=2)
# Vector de medias específico a probar
mu0   <- as.vector(c(70,170))
# Vector de medias
xbar  <- as.vector(apply(data,2,mean))
xbar
# Calculamos el estadístico de prueba:
X0   <- n*t((xbar-mu0)) %*% solve(Sigma) %*% (xbar-mu0)
round(X0,2)
# Suponiendo α=0.05, calculamos el valor crítico:

a   <- 0.05
Xc  <- qchisq(1-a,2)
round(Xc,2)
# Si quisieramos concluir la prueba por medio del p-value, sería:

p_value <- 1-pchisq(X0,2)
p_value

# pregunta 2
# Información del problema
n        <- 20
p        <- 3
# Vector de medias específico a probar
mu0   <- as.vector(c(12,4,2))
# Vector de medias muestral
xbar  <- as.vector(c(11.5,4.3,1.2))
# Matriz de covarianza muestral
S <- matrix(c(10,4,-5,4,12,-3,-5,-3,4),ncol=3)

# Calculamos el estadístico de prueba:
T0   <- n*t((xbar-mu0)) %*% solve(S) %*% (xbar-mu0)
F0   <- ((n-p)/(p*(n-1)))*T0
round(F0,2)

#Suponiendo que α=0.03,calculamos el valor crítico:
a     <- 0.03
Fc    <- qf(1-a,p,n-p)
round(Fc,2)

# p-value, debemos calcularlo:
p_value <- 1-pf(F0,p,n-p)
p_value



# pregunta 3.
# Información
n1     <- 32
n2     <- 32
p      <- 4
# Vectores de medias muestrales
xbar1    <- as.vector(c(15.97,15.91,27.19,22.75))
xbar2    <- as.vector(c(12.34,13.91,16.59,21.94))
# Matrices de covarianza muestrales
S1     <- matrix(c(
  5.192, 4.545, 6.522, 5.250,
  4.545, 13.18, 6.760, 6.266,
  6.522, 6.760, 28.67, 14.47,
  5.250, 6.266, 14.47, 16.65), 
  nrow = 4, byrow = TRUE)
S2     <- matrix(c(
  9.136, 7.549, 5.531, 4.151,
  7.549, 18.60, 5.446, 5.446,
  5.531, 5.446, 13.55, 13.55,
  4.151, 5.446, 13.55, 28.00), 
  nrow = 4, byrow = TRUE)

# Calculamos la matriz de covarianza conjunta.

Sp  <- ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)
Sp

# El estadístico es:
  
dif    <- xbar1 - xbar2
T0     <-  ((n1*n2)/(n1+n2))*t(dif)%*%solve(Sp)%*%dif
T0

# Entonces, el valor de prueba es:
F0     <- ((n1+n2-2-p)/(p*(n1+n2-2-1)))*T0
round(F0,2)

# Suponiendo que α=0.05, calculamos el valor crítico:
a     <- 0.05
Fc    <- qf(1-a,p,(n1+n2-2-p))
round(Fc,2)











