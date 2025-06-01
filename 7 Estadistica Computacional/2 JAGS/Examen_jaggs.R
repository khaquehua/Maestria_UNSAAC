## Examen de una regresión lineal JAGGS
## Kevin Heberth Haquehua Apaza
## CODIGO: 243340
##  Y=a + bX +e

set.seed(243340) # Semilla con código
n<-500 #Observaciones
x<-1:n   # valores de la variable independiente
x
epsilon <-rnorm(n,0,1) # Error aleatorio con media cero y desviación 1
y <- 2.6-8.5*x+epsilon   ### Modelo de regresión lineal simple
y

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

#Librerías uso de JAGS
library(rjags)
library(R2jags)

# Estableciendo los valores iniciales de los parámetros del modelo
parametros <- c("a","b","tau", "sigma")   # son los parámetros a ser estimados
# tau es el termino de precision o variabilidad que depende de sigma
jags1 <- jags(data = sim.dat.jags,
              inits = NULL,
              parametros,
              n.iter = 2000, # Ajuste a 2000 iteraciones
              model.file = modelo,
              n.burnin = 300, # Ajuste burnin de 300 iteraciones
              n.chains = 1,
              n.thin = 1)

summary(jags1)

#Librerías para evaluar los diagnósticos de convergencia solicitados
library(ggmcmc)
library(coda)

# Transformar las variables
jags1.mcmc <- as.mcmc(jags1)
M <- ggs(jags1.mcmc)

# Resumen
jags1$BUGSoutput$summary

#Mostremos las cadenas de Markov
traceplot <- ggs_traceplot(M) 
traceplot

#Mostremos la media ergódica
ergodica <- ggs_running(M)  
ergodica

#Mostrar la función de autocorrelación
autocor <- ggs_autocorrelation(M) 
autocor

#Mostrar la curva de densidad de Kernel
densidad <- ggs_density(M)
densidad

#Medidas descriptivas resumen
jags1$BUGSoutput$summary["a",]
jags1$BUGSoutput$summary["b",]
jags1$BUGSoutput$summary["tau",]
