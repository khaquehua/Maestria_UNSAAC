######################################################
#  ANáLISIS DE REGRESIóN LOGíSTICA                   #
######################################################
#---------------------------------------------------------
#  Librerias
library(MASS) 
library(ROCR)
library(pROC)
library(ResourceSelection)
library(pscl)

###############################
# CASO 1: EMPRESAS            #
###############################

# Preparaci?n de los datos
# Lectura de datos
library(readxl)
datosd <- read_excel("Empresas1.xlsx")
head(datosd)
View(datosd)
str(datosd)
# publicid = gasto en publicidad
# gradnove = grado de novedad
# tipo = tipo de consumo Consumo Industrial","Consumo final
# imasd = la empresa "No tiene dpto. de investigaci?n","Si tiene dpto. de investigaci?n")
# sectecng: intensidad de tecnologia utilizada


attach(datosd)
is.data.frame(datosd)

datosd$éxito   <- factor(datosd$éxito,levels=c(0,1),
                          labels=c("Fracaso","Exito"))
datosd$gradnove <- factor(datosd$gradnove,levels=c(1,2),
                          labels=c("Mejoras Sustanciales","Prod.nuevos"))
datosd$tipo <- factor(datosd$tipo,levels=c(0,1),
                          labels=c("Consumo Industrial","Consumo final"))
datosd$imasd <- factor(datosd$imasd,levels=c(0,1),
                          labels=c("No tiene dpto. de investigación","Si tiene dpto. de investigación"))
datosd$sectecng <- factor(datosd$sectecng,levels=c(1,2,3),
                          labels=c("Baja","Media","Alta"))

str(datosd)
head(datosd,8)
View(datosd)


# Para ver la categoría de referencia
contrasts(datosd$éxito)  
contrasts(datosd$gradnove)

contrasts(datosd$tipo)
contrasts(datosd$imasd)
contrasts(datosd$sectecng)
?contrasts

# Para cambiar la categor?a de referencia.
datosd$gradnove = relevel(datosd$gradnove,ref="Prod.nuevos")
datosd$sectecng = relevel(datosd$sectecng,ref="Alta")
datosd$tipo = relevel(datosd$tipo,ref="Consumo final")
datosd$imasd = relevel(datosd$imasd,ref="Si tiene dpto. de investigación")

# Para ver la categor?a de referencia
contrasts(datosd$éxito)  
contrasts(datosd$gradnove)
contrasts(datosd$tipo)
contrasts(datosd$imasd)
contrasts(datosd$sectecng)

# Modelo logistico
head(datosd)
modelo <- glm(éxito~., 
               family=binomial,
               data=datosd)

summary(modelo)
summary(modelo)$coef
coef(modelo)
modelo$y
str(modelo)

#CALIBRACI?N DEL MODELO
#PRUEBA DE HOSMER LEMESHOW

library(ResourceSelection)
hl <- hoslem.test(modelo$y, fitted(modelo),g=10) # g=10, son la cantidad de cortes para hacer la separacion en intervalos
hl
?hoslem.test
# como pvalor>0.05 el modelo ajustado es el adecuado
# la reg logistica es adecuada para el conjunto de datos

fitted(modelo)
modelo$y
cbind(hl$observed,hl$expected)
# esto es similar a realizar una Chi-cuadrado

# otra forma de calcular lo anterior
# Los 10 deciles
library(vcdExtra)
HL=HLtest(modelo,g=10)
HL
HL$table

#---------------------------------------------------------
# Cociente de ventajas (Odd Ratio)
exp(coef(modelo))
cbind(Coeficientes=modelo$coef,ExpB=exp(modelo$coef))

#                                      Coeficientes         ExpB
# (Intercept)                           -10.4705624 2.835910e-05
# publicid                                2.0219129 7.552759e+00
# gradnoveMejoras Sustanciales            1.2316069 3.426732e+00
# tipoConsumo Industrial                  0.1626351 1.176607e+00
# imasdSi tiene dpto. de investigaci?n    3.5630181 3.526948e+01
# sectecngBaja                           -2.1567646 1.156988e-01
# sectecngMedia                          -0.6857484 5.037131e-01
# personal                                0.4211636 1.523733e+00

# conclusi?n
# e^B4 = 1.17 el resultado de introducir un nuevo producto al mercado como exito(a), para 
# el tipo del producto de consumo industrial es 1.17 más ventajoso que producto de consumo final.


# e^B7 = 0.503 el resultado de introducir un nuevo producto al mercado como éxito,
# cuando la intensidad tecnol?gica del sector de actividad de la empresa es media es 0.503 veces menos 
# ventajoso que cuando la intensidad tecnologica del mercado es alta.


#----------------------------------------------------------
# Cociente de ventajas e IC 95% 
library(MASS)
round(exp(cbind(OR = coef(modelo),confint.default(modelo))), 2)

# El resultado de obtener exito al introducir un nuevo producto al mercado es 20 veces m?s ventajoso 
# por cada unidad monetaria que se invierte en publicidad

# Ho: e^B(publicidad) = 20
# H1: e^B(publicidad) <> 20

# dado que e^B(publicidad) est? en el IC [2.83 , 20.13] se acepta H0.


# Analizando puntualmente el tipo de Consumo industrial el consumo industrial es 1.18 más ventajoso que el
# consumo no industrial, sin embargo, el intervalo de confianza abarca el 1 y los efectos se podr?an indicar
# como similares para consumo industrial y no industrial.
# 

#cONTRASTE PARA LOS COEFICIENTES INDIVIDUALES
# Estad?stico W de Wald
library(survey)
regTermTest(modelo,"publicid",method="Wald")
regTermTest(modelo,"gradnove",method="Wald")
regTermTest(modelo,"tipo",method="Wald")
regTermTest(modelo,"imasd",method="Wald")
regTermTest(modelo,"sectecng",method="Wald")
regTermTest(modelo,"personal",method="Wald")
# la variable tipo no es significativo en el modelo. mientras 
# que todas las demas si son significativos para el modelo

#----------------------------------------------------------
# Devianza

anova(modelo, test="Chisq")

#EVALUANDO EL AJUSTE DEL MODELO
# Pseudo R cuadrado
library(pscl)
pR2(modelo)

# library(BaylorEdPsych)
# PseudoR2(modelo)


#----------------------------------------------------------
# Importancia de las variables
library(caret)
varImp(modelo)

#----------------------------------------------------------
#  Selecci?n de Variables  
step <- stepAIC(modelo,direction="backward", trace=FALSE)
step$anova


#----------------------------------------------------------
#  Probabilidades y grupo estimados  
proba.pred=predict(modelo,type="response")
#Probabilidades estimadas de pertenecer a la clase 1 (no referencia)
head(proba.pred)
clase.pred <- ifelse(proba.pred >= 0.5, 1, 0)
head(clase.pred)

finaldata = cbind(proba.pred,clase.pred)
head(finaldata,10)
str(finaldata)
finaldata1=as.data.frame(finaldata)

ggplot(finaldata1, aes(x = proba.pred, fill = éxito)) + geom_histogram(alpha = 0.25)
ggplot(finaldata1, aes(x = proba.pred, fill = éxito)) + geom_density(alpha = 0.25)

write.csv(finaldata,"Compras-Logistica-Probabilidades.csv")

#------------------------------------------------------------------
#  Predicci?n para nuevos individuos  

nuevo1<-data.frame(publicid=4,gradnove="Prod.nuevos",tipo="Consumo Industrial",
                   imasd="Si tiene dpto. de investigación",sectecng="Media",personal=10)
predict(modelo,newdata=nuevo1,type="response")

# bajo este modelo, esta empresa con estas caracteristicas tendria un producto nuevo exitoso.

############################################
#  Indicadores para Evaluaci?n de Modelos  #
############################################

#############################
# 1. Tabla de clasificacion #
#############################

# library(gmodels)
# predict.fit=modelo$fitted.values
# head(predict.fit)
# predict.fit[predict.fit>=0.50]=1
# predict.fit[predict.fit<0.50]=0
# CrossTable(datosd$éxito,predict.fit,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE)
# 
# #  Probabilidades y grupo estimadas  
# proba.pred=predict(modelo,type="response")
# head(proba.pred)
# 
# clase.pred <- ifelse(proba.pred >= 0.5, 1, 0)
# head(clase.pred)
# 
# finaldata = cbind(proba.pred,clase.pred)
# head(finaldata)
# str(finaldata)
# 
# library(gmodels)
# # Tabla de clasificacion
# CrossTable(x = éxito, y = clase.pred,
#            prop.t=FALSE, prop.c=FALSE, prop.chisq = FALSE)
# 
# addmargins(table(Real=éxito,Clase_Predicha=clase.pred))
# prop.table(table(Real=éxito,Clase_Predicha=clase.pred),1)
# 
# # Calcular el accuracy
# accuracy <- mean(éxito==clase.pred) ; accuracy
# (52+81)/156
# #Calcular el error de mala clasificaci?n
# 1-accuracy

#-----------------------------------
# Usando la librer?a caret
library(caret)
a=as.factor(clase.pred)
head(a)
b=as.factor(modelo$y)
head(b)
confusionMatrix(a,b)


