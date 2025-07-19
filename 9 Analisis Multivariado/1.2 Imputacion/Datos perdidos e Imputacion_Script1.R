# Datos perdidos
# Prof. Arturo Zuñiga 

library(VIM)
library(DEoptimR)
library(minqa)
library(nloptr)
library(DMwR2)
library(simputation)

# Instalación de Paquetes
# install.packages(c("VIM","DEoptimR","minqa","nloptr","DMwR", "simputation"),
#                  dependencies = c("Depends"))

#########################################################
#  Diagnóstico de datos perdidos                        
#########################################################

library(VIM)
data(tao)
head(tao)
help(tao) #https://en.wikipedia.org/wiki/Tropical_Atmosphere_Ocean_project
?tao

summary(tao) # resumen d ela base de datos

#Para ver que columnas tienen valores perdidos
which(colSums(is.na(tao))!=0)

#Para ver el porcentaje de valores perdidos en las columnas
colmiss=c(4,5,6)
per.miss.col=100*colSums(is.na(tao[,colmiss]))/dim(tao)[1]
per.miss.col

# Aggregation plot
x11()
a=aggr(tao,numbers=T)
a
summary(a)
aggr(tao,numbers=T, sortComb=TRUE, 
     sortVar=TRUE, only.miss=TRUE) 
## para modificar y ordenar el grafico .etc

# Matrix plot
x11()
matrixplot(tao)  # propuesta Dr acuña y el autor de bind

#########################################################
#  Mecanismo: ¿MCAR o MAR? es mar                              
#########################################################

#Parallel boxplots
# recuerda las columnas que presentan missing son 4, 5 y 6
VIM::pbox(tao[4:6], pos=1) #pos =1: voy a mostrar la distribucion de la primera variable
x11()
VIM::pbox(tao[4:6], pos=1)  # esto ya es cuando entrenas el ojo 

# Prueba t de medias
t.test(Sea.Surface.Temp ~ is.na(Humidity), data=tao)  # se rechaza Ho. 
# se observa que la temperatura de la superficie del mar es diferente entre las medias de la superficie cuando divides la data de humedad en ausentes y no ausentes

# Gráficos de dispersión
x11()
marginplot(tao[,c("Air.Temp", "Humidity")])

#########################################################
#  Eliminación de casos (solo si es trivial) 1% de datos            #
#########################################################
tao.cl=na.omit(tao)


#########################################################
#  Imputación                                           
#########################################################

#---------------------------------------
# Usando una medida de Tendencia Central 1% al 5%
#---------------------------------------
library(DMwR2)
tao.c<-centralImputation(tao)
summary(tao.c)
tao.d<-initialise(tao,method="median")
summary(tao.d)

par(mfrow=c(2,1))
plot(density(tao$Air.Temp, na.rm = T))
plot(density(tao.c$Air.Temp))
median(tao$Air.Temp, na.rm = T)


#---------------------------------------
# Usando Modelos de Regresión      5% al 15%
#---------------------------------------

library(simputation)
## Reemplazando por la media
tao.i <- impute_lm(tao, Air.Temp + Humidity ~ 1)
tao[c(108:110, 463,551:552),]
mean(tao$Air.Temp, na.rm = TRUE)
mean(tao$Humidity, na.rm = TRUE)
tao.i[c(108:110, 463,551:552),]

par(mfrow=c(2,1))
plot(density(tao$Air.Temp, na.rm = T))
plot(density(tao.i$Air.Temp, na.rm = T))

par(mfrow=c(2,1))
plot(density(tao$Humidity, na.rm = T))
plot(density(tao.i$Humidity, na.rm = T))

## Reemplazando por la media de cada a?o
tao.i <- impute_lm(tao, Air.Temp + Humidity ~ 1 | Year)  # si tengo pocos datos perdidos 
tao[c(108:110, 463,551:552),]
tao.i[c(108:110, 463,551:552),]

par(mfrow=c(2,1))
plot(density(tao$Air.Temp, na.rm = T))
plot(density(tao.i$Air.Temp, na.rm = T))

par(mfrow=c(2,1))
plot(density(tao$Humidity, na.rm = T))
plot(density(tao.i$Humidity, na.rm = T))

## Considerando otras variables como predictoras
tao.i <- impute_lm(tao, Air.Temp + Humidity ~ Sea.Surface.Temp + UWind + VWind | Year)  # talvez deberia ser solo sea.surface.Temp
tao[c(108:110, 463,551:552),]
tao.i[c(108:110, 463,551:552),]

par(mfrow=c(2,1))
plot(density(tao$Air.Temp, na.rm = T))
plot(density(tao.i$Air.Temp, na.rm = T))

par(mfrow=c(2,1))
plot(density(tao$Humidity, na.rm = T))
plot(density(tao.i$Humidity, na.rm = T))

#### talvez este serai el mas adecuado
#tao.i <- impute_lm(tao, Air.Temp + Humidity ~ Sea.Surface.Temp + UWind + VWind| Year)


## Adicionando un residuo aleatorio
tao.i <- impute_lm(tao, Air.Temp + Humidity ~ Sea.Surface.Temp + UWind + VWind, add_residual = "normal") ## es como poner prediction en regresion
tao[c(108:110, 463,551:552),]
tao.i[c(108:110, 463,551:552),]

par(mfrow=c(2,1))
plot(density(tao$Air.Temp, na.rm = T))
plot(density(tao.i$Air.Temp, na.rm = T))

par(mfrow=c(2,1))
plot(density(tao$Humidity, na.rm = T))
plot(density(tao.i$Humidity, na.rm = T))

#---------------------------------------
# K-Vecinos m?s cercanos                 
#---------------------------------------

# Usando la libreria VIM
tao_vars <- c("Air.Temp","Humidity")
tao_i_knn <- VIM::kNN(data=tao, variable=tao_vars)

tao[c(108:110, 463,551:552),]
tao_i_knn[c(108:110, 463,551:552),]
x11()
aggr(tao_i_knn, delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))
x11()
aggr(tao_i_knn,delimiter="_imp",numbers=TRUE, prop=c(TRUE,FALSE), combined = TRUE)

barMiss(tao_i_knn, delimiter="imp", selection="any")

# Usando la libreria DMwR
tao_i_knn2<-DMwR2::knnImputation(tao)
tao_i_knn2[c(108:110, 463,551:552),]





