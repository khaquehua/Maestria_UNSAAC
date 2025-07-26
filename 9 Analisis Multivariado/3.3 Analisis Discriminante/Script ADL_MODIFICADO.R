# =============================================== #
#        prof. Arturo Zuñiga Blanco               #
#          arturo.zuniga@unsaac.edu.pe            #
# =============================================== # 

#######################
# 1. Lectura de datos #
#######################

library(foreign)
datosd=read.spss("datos_AD_Simple.sav",
                 to.data.frame = TRUE,
                 use.value.labels = TRUE)
head(datosd) # es necesario que datosd sea un data frame

attach(datosd) # reconoce a las cabeceras de la data


#########################################################
# 2. Análisis Descriptivo de las variables predictoras  #
#########################################################

library(psych)
psych::describe(datosd)
summary(datosd)

tapply(MON_PRE,TIPO,mean)
tapply(PAT_NET,TIPO,mean)
tapply(DEU_PEN,TIPO,mean)


###########################
#  Analisis Exploratorio  #
###########################

par(mfrow=c(1,3))
boxplot(MON_PRE~TIPO,col="green",data=datosd,main="monto del prestamos")
boxplot(PAT_NET~TIPO,col="gold",data=datosd,main="patrimonio neto")
boxplot(DEU_PEN~TIPO,col="darkred",data=datosd,main="Deuda pendiente")

# ====================================================
#              VALIDACION DE SUPUESTOS

# Supuesto de homogeneidad de matrices variancia covariancia
library(biotools)
head(datosd)
boxM(datosd[ ,c(2,3,4)], datosd$TIPO)
boxM(datosd[,-1],datosd[,1]) # Primero las Xs y luego la Y
# como p_valor es >0.05 aceptamos Ho y decimos que son 
# homogeneas las matrices de varianzas y cov entre los distintos grupos


#Supuesto del poder discriminante de cada variable
mod.dca = lm(MON_PRE ~ TIPO) # modelo DCA
summary(aov(mod.dca)) # Análisis de varianza
t.test(MON_PRE~ TIPO, alternative = "t", var.equal =T)

mod.dca = lm(PAT_NET ~ TIPO) # modelo DCA
summary(aov(mod.dca)) # Análisis de varianza

mod.dca = lm(DEU_PEN ~ TIPO) # modelo DCA
summary(aov(mod.dca)) # Análisis de varianza

# Supuesto de normalidad multivariada de cada grupo
#Desagregamos la base por grupos
grupo1=datosd[datosd$TIPO=="Fallidos", 2:4]
grupo2=datosd[datosd$TIPO=="No fallidos", 2:4]
grupo1=t(grupo1) # t significa la transpuesta
grupo2=t(grupo2)

library(mvnormtest)
mshapiro.test(grupo1) # como pvalor>0.05, se dice q hay normalida multivariada para grupo 1
mshapiro.test(grupo2)

###########################
# Selección de Variables  #
###########################

# Criterio de lambda de Wilks para seleccion de variables
library(klaR)
greedy.wilks(TIPO ~ .,data=datosd)
# fijarse el pvalor overall, que es el indicar
# sobre todos los datos y con todas las variables
# si el pvalor <0.05, las variables son significativa
# para realizar LDA

# otro criterio para seleccionar variables con validacion cruzada y reggresion
library(MASS)
stepclass(datosd[,2:4],datosd$TIPO,method="lda")

###################################################
#  Estimacion de la Funcion Discriminante Lineal  #
###################################################
# decidi por el criterio de lambda de Wilks y conservo las 3 variables
library(MASS)
modelo<-lda(TIPO ~ MON_PRE + PAT_NET + DEU_PEN,datosd,prior=c(1,1)/2)
print(modelo)

D1 = -0.3687313*8.8125+0.3117728*5-0.2944190*5
D1 # fallidos
D2 = -0.3687313*5.0875 +0.3117728*9-0.2944190*3
D2 # no fallidos

C=(D1+D2)/2 ;C

# prueba
-0.3687313*12.5 +0.3117728*1.3-0.2944190*4.1  # -5.410955 <-1.55 por tanto es Fallido
-0.3687313*3.6 +0.3117728*6.3-0.2944190*1.6  # -0.8942 >-1.55, No fallidos


clase.pred <-predict(modelo,datosd[,-1])$class
clase.pred

proba.pred  <-predict(modelo,datosd[,-1])$posterior
proba.pred
str(proba.pred)
proba.pred <- proba.pred[,2]

# accuracy # tasa de buena clasificacion
library(caret)
confusionMatrix(clase.pred,datosd$TIPO,positive="No fallidos")

#######################################
#  Predicción para nuevos individuos  #
#######################################


nuevo1<-data.frame(MON_PRE=12,PAT_NET=6,DEU_PEN=5)
predict(modelo,nuevo1)$class
predict(modelo,nuevo1)$posterior

nuevo2<-data.frame(MON_PRE=5,PAT_NET=9,DEU_PEN=3)
predict(modelo,nuevo2)$class
predict(modelo,nuevo2)$posterior

NUEVO = read.csv("DATA_MONCHITO.csv", header = T, sep = ";")
head(NUEVO)
A=predict(modelo,NUEVO)$class
B=predict(modelo,NUEVO)$posterior

datanueva= cbind(NUEVO,A,B)
datanueva

#-------------------------------------------------------------
# Almacenamiento de datos con clase y probabilidad predecida
write.csv(datanueva,"DATA_FINAL2022.csv")



# algunas cosas extras
library(psych)
pairs.panels(datosd[,2:4],
             bg=c("red","yellow")[datosd$TIPO],pch=21)
#
partimat(TIPO~.,data=datosd,method="lda",nplots.vert=2)


# pregunta de alumno
# como saber que variable influye mas en este analisis discriminante
cor(datosd$MON_PRE,proba.pred)
cor(datosd$PAT_NET,proba.pred)
cor(datosd$DEU_PEN,proba.pred)
