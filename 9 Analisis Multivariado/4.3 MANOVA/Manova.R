################################################
#  MANOVA CON R                      #
################################################

rm(list = ls())
# MANOVA EN DCA

    
# Ingreso de datos
# Lectura de datos SPSS

library(foreign)
datos <-read.spss("Maiz.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)
str(datos)
head(datos)

# Supuesto de normalidad multivariada
#La siguiente funci?n (mshapiro.test) se aplica grupo a grupo, por lo que primero es necesario
#segregar la base de datos en los grupos, seis en el ejemplo.

#desagregamos la base por grupos.
datosc=datos[,-7]
head(datosc)

hibrido1=datosc[datosc$Tratamientos=="Híbrido1",2:6]
hibrido1
hibrido2=datosc[datosc$Tratamientos=="Híbrido2",2:6]
hibrido2
hibrido3=datosc[datosc$Tratamientos=="Híbrido3",2:6]
hibrido3
hibrido4=datosc[datosc$Tratamientos=="Híbrido4",2:6]
hibrido4
hibrido5=datosc[datosc$Tratamientos=="Híbrido5",2:6]
hibrido5
hibrido6=datosc[datosc$Tratamientos=="Híbrido6",2:6]
hibrido6

#El test necesita las variables en filas, entonces hallamos las transpuestas.
hibrido1=t(hibrido1)
hibrido2=t(hibrido2)
hibrido3=t(hibrido3)
hibrido4=t(hibrido4)
hibrido5=t(hibrido5)
hibrido6=t(hibrido6)

#Ejecutamos el test

library(mvnormtest)
mshapiro.test(hibrido1)
mshapiro.test(hibrido2)
mshapiro.test(hibrido3)
mshapiro.test(hibrido4)
mshapiro.test(hibrido5)
mshapiro.test(hibrido6)
#Con cada h?brido no hay normalidad entonces 
# deber?an plantearse transformaciones o 
# usar las pruebas
# mas robustas que se describen m?s abajo.

# Supuesto de homogeneidad de matrices variancia covariancia

library(heplots)
datosc=datos[,-7]
str(datosc)
head(datosc)

boxM(cbind(Y1,Y2,Y3,Y4,Y5)~Tratamientos,data=datosc)

# Ho: V1=V2=V3=V4=V5=V6
# Ha: alguna es diferente

# Box's M-test for Homogeneity of Covariance Matrices
# 
# data:  Y
# Chi-Sq (approx.) = 95.967, df = 75, p-value = 0.05181
# interpretacion:NO SE RECHAZA hO, A UN NIVEL DE SIGNIFICANCIA 5%  POR TANTO LAS VARIANZAS SON PARECIDAS HOMOGENEAS


library(biotools)
biotools::boxM(datos[c(-1,-7)],datos[,1])


# Supuesto de variables dependientes correlacionadas. Prueba de esfericidad de Bartlett

library(psych)

datos1=datos[c(-1,-7)]
options(scipen=0)
cortest.bartlett(cor(datos1),n=dim(datos1))
# estan correlacionadas por tanto se puede 
# realizar manova, de no cumplirse este supuesto 
# habria que realizar 5 anovasen dca

#Trabajando con el modelo de MANOVA en DCA
datosc=datos[,-7]
str(datosc)
head(datosc)
datosc
modelo=manova(cbind(Y1,Y2,Y3,Y4,Y5)~Tratamientos,data=datosc)
summary(modelo)

#Determinaci?n de la matriz residual y la matriz factorial del MANOVA.
str(modelo)
Matrices=summary(modelo)$SS
F=Matrices$Tratamientos
W=Matrices$Residuals

#Variabilidad explicada por el factor (Tratamientos). 
# Matriz suma de cuadrados y productos cruzados
# del factor (SCOCF)
F

#Variabilidad residual. Matriz suma de cuadrados y productos
# cruzados
# del residual (SCOCR)
W

#Variabilidad Total. Matriz suma de cuadrados y productos cruzados total (SCOCT)
#del factor 
T=F+W
T

#Bondad de ajuste. Un valor pr?ximo a 1 indica que
# la mayor parte de la variabilidad total puede
#atribuirse al factor, mientras que un valor pr?ximo
# a 0 significa que el factor explica muy poco
#de esa variabilidad total.

eta2=1-det(W)/det(T)
eta2
# 0.7606161: el 76% de la variabilidad  total se le atribuye a la variedad o factor


# Pruebas de hip?tesis del modelo. C?lculo de contrastes del modelo

summary(modelo,test="Pillai")  #   como p_valor menor a alfa se rechaza Ho,por tanto las medias de las Ys en cada factor es diferente                                                                                                                                       
summary(modelo,test="Wilks")
summary(modelo,test="Hotelling-Lawley")
summary(modelo,test="Roy")
summary.aov(modelo)

#Contrastes del modelo en relaci?n a los supuestos
#Todos los estad?sticos son bastante robustos ante violaciones de normalidad, y la prueba de Roy es muy
#sensible a violaciones de la hip?tesis de la matriz de covariancias. Cuando las muestras son iguales
#por grupo, la prueba de Pillai es el estad?stico m?s robusto ante violaciones de los supuestos.

#Comparar dos h?bridos

modelo1=manova(cbind(Y1,Y2,Y3,Y4,Y5)~Tratamientos,data=datosc,
               subset=Tratamientos %in% c("Híbrido1","Híbrido2"))

summary(modelo1,test="Pillai")
summary(modelo1,test="Wilks")
summary(modelo1,test="Hotelling-Lawley")
summary(modelo1,test="Roy")
summary.aov(modelo1)

# estamos comparando solo dos hibridos 1 y 2; y entre estos no habra diferencias significativas


modelo2=manova(cbind(Y1,Y2,Y3,Y4,Y5)~Tratamientos,data=datosc,
               subset=Tratamientos %in% c("Híbrido1","Híbrido6"))

summary(modelo2,test="Pillai")
summary(modelo2,test="Wilks")
summary(modelo2,test="Hotelling-Lawley")
summary(modelo2,test="Roy")
summary.aov(modelo2)

 

