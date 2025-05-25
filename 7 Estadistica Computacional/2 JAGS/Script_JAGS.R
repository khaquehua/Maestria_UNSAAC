## Script para uso de la libreria JAGS

modelo_beta_binomial <- function(){
  for (i in 1:N) {
    y[i] ~ dbin(theta[i], n[i])
    theta[i] ~ dbeta(alpha, beta)
  }
  
  # Prioris para alpha y beta
  alpha ~ dgamma(1, 1)
  beta ~ dgamma(1, 1)
  
}

# Datos ficticios
y <- c(5, 7, 10, 3, 8)         # Exitos
n <- c(10, 10, 10, 10, 10)     # Ensayos

data_jags <- list(
  y = y,
  n = n,
  N = length(y)
)

# Valores iniciales de los parámetros alpha y beta
inits <- function() {
  list(alpha = 1, beta = 1)
}

library(rjags)
library(R2jags)

parametros <- c("theta","alpha","beta") # son
jags1 <- jags(
  data = data_jags,
  inits = inits,
  parametros,
  n.iter = 1000,
  model.file = modelo_beta_binomial,
  n.burnin = 200,
  n.chains = 1,
  n.thin = 1
)

summary(jags1)
jags1$BUGSoutput

# Estadísticas descriptivas distribuciones a posteriori
jags1$BUGSoutput$summary

# Estadísticas descriptivas distribuciones a posteriori (individual)
jags1$BUGSoutput$summary["alpha", ]
jags1$BUGSoutput$summary["beta", ]
jags1$BUGSoutput$summary["deviance", ]
jags1$BUGSoutput$summary["theta[1]", ]
jags1$BUGSoutput$summary["theta[2]", ]

# DIC y número efectivo de parámetros
jags1$BUGSoutput$DIC

# Diagnósticos de convergencia
library(ggmcmc)
library(coda)

jags1.mcmc <- as.mcmc(jags1)
M <- ggs(jags1.mcmc)

# Diagnósticos gráficos
densidad <- ggs_density(M) ## densidad de los parámetros esetimados
densidad
traceplot <- ggs_traceplot(M) ## cadenas de los parámetros estimados
traceplot
autocor <- ggs_autocorrelation(M) ## autocorrelación de los parámetros
autocor
ergodica <- ggs_running(M) ## Media ergódica de los parámetros estimado
ergodica

## Intervalo de credibilidad e intervalo HPD para los parámetros
sims <- jags1.mcmc
sims <- as.matrix(sims)

## Parámetro a
quantile(sims[,1], c(0.025, 0.975))






