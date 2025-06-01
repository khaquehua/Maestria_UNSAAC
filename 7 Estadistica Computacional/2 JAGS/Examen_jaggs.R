## Ejemplo simulado de una regresión lineal
##  Y=a + bX +e

set.seed(1)
n<-500
x<-1:n   # valores de la variable independiente
x
epsilon <-rnorm(n,0,1)
y <- 2-5*x+epsilon   ### Modelo de regresión lineal simple
y
#yy <- 2-5*1-0.626453811
#yy

example1<-data.frame(x=x,y=y,Epsilon=epsilon)
example1


#Modelo
modelo <-function()
{
  for(i in 1:n){
    
    y[i] ~ dnorm(y.hat[i],tau)
    y.hat[i] <- a+b*x[i]
  }
  # Distribuciones a priori
  a ~ dnorm(0,0.0001)
  b ~ dnorm(0,0.0001)
  tau <- pow(sigma,-2)
  sigma ~ dunif(0,100)
}

# Crear base de datos para el modelo JAGS

sim.dat.jags <-list(
  n=nrow(example1),
  x=x,
  y=y
)

library(rjags)
library(R2jags)

# Estableciendo los valores iniciales de los parámetros del modelo
parametros <- c("a","b","tau", "sigma")   # son los parámetros a ser estimados
# tau es el termino de precision o variabilidad que depende de sigma
jags1 <- jags(data = sim.dat.jags,
              inits = NULL,
              parametros,
              n.iter = 1000,
              model.file = modelo,
              n.burnin = 200,
              n.chains = 1,
              n.thin = 1)

summary(jags1)

# Estad?sticas descriptivas distribuciones a posteriori
jags1$BUGSoutput$summary

# Estad?sticas descriptivas distribuciones a posteriori (individual)
jags1$BUGSoutput$summary["a",]
jags1$BUGSoutput$summary["b",]
jags1$BUGSoutput$summary["tau",]


# DIC y n?mero efectivo de par?metros
jags1$BUGSoutput$DIC

# Diagn?sticos de convergencia
library(ggmcmc)
library(coda)
jags1.mcmc <- as.mcmc(jags1)
M <- ggs(jags1.mcmc)

# Diagn?sticos gr?ficos
densidad <- ggs_density(M)   ## densidad de los par?metros estimados
densidad
traceplot <- ggs_traceplot(M)  ## cadenas de los par?metros estimados
traceplot
autocor <- ggs_autocorrelation(M) ## autocorrelaci?n de los par?metros estimados
autocor
ergodica <- ggs_running(M)  ## media erg?dica de los par?metros estimados
ergodica

## Intervalo de credibilidad e intervalo HPD para los par?metros 
sims <-jags1.mcmc
sims <- as.matrix(sims)
## Par?metro a
quantile(sims[,1],c(0.025,0.975))
HPDinterval(as.mcmc(sims[,1]),prob = 0.95)

## Par?metro b
quantile(sims[,2],c(0.025,0.975))
HPDinterval(as.mcmc(sims[,2]),prob = 0.95)

## Par?metro tau


### Considerando dos cadenas para el mismo ejemplo de la regresión lineal simple
# Estableciendo los valores iniciales de los parámetros del modelo
parametros <- c("a","b","tau", "sigma")   # son los parámetros a ser estimados
jags1 <- jags(data = sim.dat.jags,
              inits = NULL,
              parametros,
              n.iter = 1000,
              model.file = modelo,
              n.burnin = 200,
              n.chains = 3,
              n.thin = 1)

summary(jags1)

# Estad?sticas descriptivas distribuciones a posteriori
jags1$BUGSoutput$summary

# Estad?sticas descriptivas distribuciones a posteriori (individual)
jags1$BUGSoutput$summary["a",]
jags1$BUGSoutput$summary["b",]
jags1$BUGSoutput$summary["tau",]


# DIC y n?mero efectivo de par?metros
jags1$BUGSoutput$DIC

# Gr?ficas de las distribuciones a posteriori
par(mfrow=c(2,2))
plot(density(jags1$BUGSoutput$sims.list$a))
plot(density(jags1$BUGSoutput$sims.list$b))
plot(density(jags1$BUGSoutput$sims.list$tau))

# Diagn?sticos de convergencia
library(ggmcmc)
library(coda)
jags1.mcmc <- as.mcmc(jags1)
M <- ggs(jags1.mcmc)

# Diagn?sticos gr?ficos
densidad <- ggs_density(M)   ## densidad de los par?metros estimados
densidad
traceplot <- ggs_traceplot(M)  ## cadenas de los par?metros estimados
traceplot
autocor <- ggs_autocorrelation(M) ## autocorrelaci?n de los par?metros estimados
autocor
ergodica <- ggs_running(M)  ## media erg?dica de los par?metros estimados
ergodica