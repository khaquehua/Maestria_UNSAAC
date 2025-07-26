##################################################
#                                                #
# AN?LISIS DISCRIMINANTE LINEAL                  #
#      EJEMPLO EMPRESAS                       #
##################################################
#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

##############
#  Paquetes  #
##############

library(psych)
library(MASS)
library(klaR)
library(gains)
library(caret)
library(Boruta) 
library(gmodels)

######################
#  Lectura de Datos  #
######################

# Lectura de datos con 5 variables
library(foreign)
datosd <-read.spss("Empresas.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)
str(datosd)
head(datosd)

#########################################################
# 2. An?lisis Descriptivo de las variables predictoras  #
#########################################################

library(psych)
summary(datosd)
attach(datosd)
tapply(inc_prom,resultad,mean)
tapply(inc_publ,resultad,mean)
tapply(patrocin,resultad,mean)
tapply(promocio,resultad,mean)
tapply(publicid,resultad,mean)

###########################
#  Analisis Exploratorio  #
###########################
x11()
par(mfrow=c(3,2))
boxplot(inc_prom~resultad,col="green",data=datosd,main="inc_prom")
boxplot(inc_publ~resultad,col=8,data=datosd,main="inc_publ")
boxplot(patrocin~resultad,col=22,data=datosd,main="patrocin")
boxplot(promocio~resultad,col=10,data=datosd,main="promocio")
boxplot(publicid~resultad,col=4,data=datosd,main="publicid")
par(mfrow=c(1,1))
###########################
# Selecci?n de Variables  #
###########################

# Criterio de lambda de Wilks para seleccion de variables
library(klaR)
greedy.wilks(resultad ~ .,data=datosd)

# Selecci?n de variables con la librer?a Boruta
library(Boruta) 
set.seed(123)
boruta <- Boruta(resultad~., data = datosd, doTrace = 2)
print(boruta)

plot(boruta,cex.axis=0.5)

#TRABAJANDO SEG?N EL CRITERIO DE LAMDA DE WILKS

datosd=datosd[,-2:-3] # ELIMINAR incr publi y incr promocion
head(datosd)

# Supuesto de homogeneidad de matrices variancia covariancia

library(biotools)

boxM(datosd[,-1],datosd[,1])  
# como pvalor>0.05 no se rechaza la hipotesis planteada
#♣ existe homogeneidad de varianzas.


#Supuesto del poder discriminante de cada variable

mod.dca = lm(patrocin ~ resultad,datosd) # modelo DCA
summary(aov(mod.dca)) # An?lisis de varianza
# patrocinio es una buena variable discriminadora

mod.dca = lm(promocio ~ resultad,datosd) # modelo DCA
summary(aov(mod.dca)) # An?lisis de varianza
# promocion es una buena variable discriminadora

mod.dca = lm(publicid ~ resultad,datosd) # modelo DCA
summary(aov(mod.dca)) # An?lisis de varianza
# comentarios: 
# al parecer las tres variables consideradas, presentan diferencias
# significativas con respecto a los 3 distintos grupos de empresa.


# Supuesto de normalidad multivariada de cada grupo

#Desagregamos la base por grupos, transponemos y efectuamos la prueba.
datosd
grupo1=datosd[datosd$resultad=="Bajos",2:4]
grupo2=datosd[datosd$resultad=="Medios",2:4]
grupo3=datosd[datosd$resultad=="Altos",2:4]

grupo1=t(grupo1)
grupo2=t(grupo2)
grupo3=t(grupo3)

library(mvnormtest)
mshapiro.test(grupo1) #existe normalidad
mshapiro.test(grupo2)  # existe normalidad
mshapiro.test(grupo3)  #no existe normalidad

###################################################
#  Estimacion de la Funcion Discriminante Lineal  #
###################################################
modelo<-lda(resultad ~patrocin+promocio+publicid,datosd,prior=c(1,1,1)/3)
print(modelo)

20.97778*-0.3087337 + 156.8889*0.3552143+ 241.1667*0.2548989
20.97778*-0.9428909 + 156.8889*0.4063704+ 241.1667*-0.1744722

21.27647*-0.3087337 + 161.7059*0.3552143+ 245.8824*0.2548989
21.27647*-0.9428909 + 161.7059*0.4063704+ 245.8824*-0.1744722

20.04286*-0.3087337 + 154.5714*0.3552143+ 235.3571*0.2548989
20.04286*-0.9428909 + 154.5714*0.4063704+ 235.3571*-0.1744722

c1=(1.898362+2.751458+2.85174)/3
c1

c2=(110.7258+113.5466+108.7103)/3
c2

#Significaci?n conjunta de las funciones discriminantes
#Las funcines discriminantes son estad?sticamente significativas
head(datosd)
fit.manova=manova(data=datosd,cbind(patrocin,promocio,publicid)~resultad)
summary((fit.manova),test="Wilks")

#GL Residuales=46

#Autovalores
summary((fit.manova),test="Wilks")$Eigenvalues  ## solo tomamos los dos primeros por ser dos FD
lambda1=4.005055
lambda2=0.2005152
eta2.1=lambda1/(1+lambda1)    #prueba de bondad de ajuste que indica la capacidad 
eta2.2=lambda2/(1+lambda2)
eta2.1
eta2.2

#El 80% de la variación total de la variable dependiente es explicada por la funci?n discriminante 1.
#El 16.7% de la variaci?n total es explicada por la funci?n discriminante 2.


#Correlaci?n can?nica
eta1=eta2.1^0.5
eta2=eta2.2^0.5
eta1
eta2
#Eta1=0.895.Como se aproxima a 1 entonces la FD1 tiene gran capacidad explicativa.
#Eta2=0.409.Como no se aproxima a 1 entonces la FD2 no tiene gran capacidad explicativa.

#GL Residuales=46


#########################################################
#  Clasificacion y Probabilidades de las observaciones  #
#########################################################

#--------------------------
# Clase predicha
head(datosd)
clase.pred <-predict(modelo,datosd[,-1])$class
clase.pred


#---------------------------
# Probabilidad predicha
proba.pred  <-predict(modelo,datosd[,-1])$posterior
proba.pred
str(proba.pred)


#-------------------------------------------------------------
# Almacenamiento de datos con clase y probabilidad predecida
datosf=cbind(datosd,clase.pred,proba.pred)
head(datosf)
tail(datosf)
datosf
str(datosf)
write.csv(datosf,"resultados-adl-scores.csv")


library(ggplot2)
proba.pred1 <- proba.pred[,2]
ggplot(datosf, aes(x = proba.pred1, fill = resultad)) + geom_histogram(alpha = 0.25)
ggplot(datosf, aes(x = proba.pred1, fill = resultad)) + geom_density(alpha = 0.25)+ylim(c(0,15))


############################################
#  Indicadores para Evaluaci?n de Modelos  #
############################################

#############################
# 1. Tabla de clasificacion #
#############################

library(gmodels)
# Tabla de clasificaci?n
CrossTable(x = resultad, y = clase.pred,
           prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)

addmargins(table(Real=resultad,Clase_Predicha=clase.pred))
prop.table(table(Real=resultad,Clase_Predicha=clase.pred),1)

# Calcular el accuracy
accuracy <- mean(resultad==clase.pred) ; accuracy
(18+15+14)/49
# Calcular el error de mala clasificaci?n
error <- mean(resultad!=clase.pred) ; error
1-(18+15+14)/49


#-----------------------------------
# Usando la librer?a caret
library(caret)
confusionMatrix(clase.pred,resultad,positive="Si")


#######################################
#  Predicci?n para nuevos individuos  #
#######################################

# publicid promocio patrocin
#   243      153      21.3

nuevo1<-data.frame(publicid=243,promocio=153,patrocin=21.3)
predict(modelo,nuevo1)$class
predict(modelo,nuevo1)$posterior

nuevo2<-data.frame(publicid=236,promocio=152,patrocin=20)
predict(modelo,nuevo2)$class
predict(modelo,nuevo2)$posterior

# nueva empresa
# publicid promocio patrocin
#   215      180      19.8
nuevo3<-data.frame(publicid=215,promocio=180,patrocin=19.8)
predict(modelo,nuevo3)$class
predict(modelo,nuevo3)$posterior

# tarea genera una data con 10 empresas nuevas en csv, y luego
# vas a preddecir con el modelo creado el tipo de empresa que seria
