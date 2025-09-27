###############################################
## Factorial 2^3 BLOQUEADO (ABC confundido)
## 3 réplicas, 2 bloques por réplica (6 bloques)
## 
###############################################

## 1) Paquetes ----
## FrF2/DoE.base dan utilidades para diseños factoriales; broom deja "tablas" con buen formato

# install.packages(c("FrF2","DoE.base","broom"))

library(FrF2)
library(DoE.base)
library(broom)

## 2) Ingresamos los datos EXACTOS de la tabla ----
## Cada fila es una corrida: niveles de A,B,C en 0/1 y la respuesta "Y" (pureza).
## También indicamos Réplica (I, II, III) y Bloque (1..6) como en la figura.

dat <- rbind(
  ## Réplica I  --------------------------
  ## Bloque 1: (ABC)_1
  data.frame(replica="I",  bloque=1, A=0, B=0, C=1, Y=44.5), # 001
  data.frame(replica="I",  bloque=1, A=0, B=1, C=0, Y=44.2), # 010
  data.frame(replica="I",  bloque=1, A=1, B=0, C=0, Y=60.1), # 100
  data.frame(replica="I",  bloque=1, A=1, B=1, C=1, Y=48.8), # 111
  
  ## Bloque 2: (ABC)_0
  data.frame(replica="I",  bloque=2, A=0, B=0, C=0, Y=46.8), # 000
  data.frame(replica="I",  bloque=2, A=0, B=1, C=1, Y=44.5), # 011
  data.frame(replica="I",  bloque=2, A=1, B=0, C=1, Y=57.0), # 101
  data.frame(replica="I",  bloque=2, A=1, B=1, C=0, Y=58.5), # 110
  
  ## Réplica II --------------------------
  ## Bloque 3: (ABC)_0
  data.frame(replica="II", bloque=3, A=1, B=0, C=1, Y=49.8), # 101
  data.frame(replica="II", bloque=3, A=1, B=1, C=0, Y=52.0), # 110
  data.frame(replica="II", bloque=3, A=0, B=1, C=1, Y=48.8), # 011
  data.frame(replica="II", bloque=3, A=0, B=0, C=0, Y=51.5), # 000
  
  ## Bloque 4: (ABC)_1
  data.frame(replica="II", bloque=4, A=0, B=0, C=1, Y=55.5), # 001
  data.frame(replica="II", bloque=4, A=1, B=0, C=0, Y=59.8), # 100
  data.frame(replica="II", bloque=4, A=0, B=1, C=0, Y=56.0), # 010
  data.frame(replica="II", bloque=4, A=1, B=1, C=1, Y=58.5), # 111
  
  ## Réplica III -------------------------
  ## Bloque 5: (ABC)_0
  data.frame(replica="III",bloque=5, A=0, B=1, C=1, Y=53.2), # 011
  data.frame(replica="III",bloque=5, A=1, B=0, C=1, Y=57.2), # 101
  data.frame(replica="III",bloque=5, A=0, B=0, C=0, Y=56.0), # 000
  data.frame(replica="III",bloque=5, A=1, B=1, C=0, Y=59.0), # 110
  
  ## Bloque 6: (ABC)_1
  data.frame(replica="III",bloque=6, A=1, B=0, C=0, Y=69.5), # 100
  data.frame(replica="III",bloque=6, A=0, B=1, C=0, Y=62.8), # 010
  data.frame(replica="III",bloque=6, A=0, B=0, C=1, Y=55.0), # 001
  data.frame(replica="III",bloque=6, A=1, B=1, C=1, Y=53.8)  # 111
)

## Chequeo rápido de tamaños (deben ser 24 corridas = 8 tratamientos * 3 réplicas)
nrow(dat)
table(dat$replica)
table(dat$bloque)

## 3) Creamos variables útiles para DOE ----
## - Factores en codificación -1/+1 (estándar para calcular efectos)
## - Factor "Blocks" (6 niveles) y "Replica" (3 niveles)
dat$Apm <- ifelse(dat$A==0, -1, 1)  # A plus-minus
dat$Bpm <- ifelse(dat$B==0, -1, 1)
dat$Cpm <- ifelse(dat$C==0, -1, 1)

dat$Blocks  <- factor(dat$bloque)     # factor de 6 niveles
dat$Replica <- factor(dat$replica, levels=c("I","II","III"))

## (Opcional didáctico) Verificamos la regla de bloqueo: (ABC)_0 vs (ABC)_1
## En 0/1: bloque tipo 0 si (A+B+C) %% 2 == 0; tipo 1 si == 1
dat$ABCparidad <- (dat$A + dat$B + dat$C) %% 2  # 0 -> (ABC)_0 ; 1 -> (ABC)_1
with(dat, tapply(ABCparidad, Blocks, unique))     # inspecciona qué paridad tiene cada bloque

## 4) Ajustamos el modelo ANOVA con bloques ----
## Importante: al confundir ABC con bloques, el efecto ABC se "va" al término Blocks.
## Por eso, en el modelo sólo incluimos Principales + Dos-factores (A, B, C, AB, AC, BC)
## y añadimos "Blocks" como efecto de bloqueo (factor fijo aquí).
fit <- aov(
  Y ~ Blocks + (Apm + Bpm + Cpm)^2,  # principales e interacciones de 2 factores
  data = dat
)

## Resumen ANOVA
summary(fit)

## 5) Tabla ordenada (broom) y estimación de efectos ----
tidy_fit <- broom::tidy(fit)
tidy_fit[order(tidy_fit$term), ]

 ## Efectos estimados a partir de un modelo lineal con codificación -1/+1
## (esto te permite ver magnitudes y signos de efectos)
fit_lm <- lm(Y ~ Blocks + (Apm + Bpm + Cpm)^2, data = dat)
coefs <- coef(summary(fit_lm))
coefs

## 6) ¿Qué pasa con ABC? (alias con bloques) ----
## No incluimos ABC en el modelo porque está "no estimable" al estar perfectamente
## confundido con Blocks: cualquier cambio sistemático de ABC es absorbido por Blocks.
## Para visualizar aliasing teórico usamos un diseño equivalente con FrF2:

## Generamos el diseño 2^3 con 3 repeticiones, 2 bloques por réplica, bloqueando con ABC
## (sin respuesta; esto es sólo para imprimir la estructura de alias)
des <- FrF2(nruns      = 8,
            nfactors   = 3,
            blocks     = 2,         # 2 bloques por réplica
            replications = 3,       # 3 réplicas -> 6 bloques
            block.gen  = "ABC",     # interacción que define bloques
            randomize  = FALSE)     # didáctico: sin aleatorizar

## Estructura de alias (respecto a principales e interacciones de 2 factores)
aliasprint(des)   # verás que ABC aparece en la columna de bloques

## 7) Gráficos de diagnóstico y de efectos ----
## (a) Gráfico de Daniel (normal de efectos) para detectar efectos "grandes"
## Nota: DanielPlot necesita un objeto aov con sólo los términos de tratamiento.
## Le quitamos Blocks para ese gráfico específico:
## Modelo solo con tratamientos (como hiciste para el DanielPlot)
fit_trt <- aov(Y ~ (Apm + Bpm + Cpm)^2, data = dat)  # sólo tratamientos
par(mfrow=c(1,1))
DanielPlot(fit_trt, main="Daniel Plot - Efectos de tratamiento (sin Blocks)")

## (b) Efectos principales
MEPlot(fit_trt, main="Efectos principales")

## (c) Interacciones de dos factores
IAPlot(fit_trt, main="Interacciones de dos factores")

## 8) Medias por tratamiento y por bloque (resumen) --
## Tratamiento = combinación -1/+1 de A,B,C
dat$trt <- with(dat, interaction(Apm,Bpm,Cpm, sep=""))
aggregate(Y ~ trt, data = dat, FUN = mean)  # medias por tratamiento
aggregate(Y ~ Blocks, data = dat, FUN = mean)  # medias por bloque (días)

## 9) Breve interpretación guiada (para el informe) --
## - El término Blocks captura variación entre días. 
##   Como ABC está confundido con Blocks,
##   no podemos separar "efecto ABC" del "efecto día". Por eso ABC NO se estima.
## - Observa en summary(fit) qué principales/interacciones 
##   2-factores resultan significativas.
## - Usa MEPlot/IAPlot para visualizar qué factores elevan la
##   pureza y cómo interactúan.

