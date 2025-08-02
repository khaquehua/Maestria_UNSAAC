#  REGRESIÓN MULTIVARIADA CON R                #

# Ejemplo de Estudio Piña
# Ingreso de datos
# Lectura de datos SPSS

library(foreign)
datos <-read.spss("9 Analisis Multivariado/4.4 Regresion multivariada/estudio_pina.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)
str(datos)
head(datos)
# Supuesto de normalidad multivariada

datos1=datos[c(-3,-4)]
head(datos1)

library(mvnormtest)
mshapiro.test(t(datos1))
# tienen distribucion normalidad multivariada

# Supuesto de variables dependientes correlacionadas.
# Prueba de esfericidad de Bartlett

library(psych)
#library(rela)
options(scipen=0)
cortest.bartlett(cor(datos1),n=dim(datos1))
# 
# $`chisq`
# [1]  6.5033516 -0.5912138  #te fijas el primer valor
# 
# $p.value
# [1] 0.01076713 1.00000000 # te fijas el primer valor
# 
# $df
# [1] 1

# # Pruebas de hipotesis separadas del modelo
datosc=datos
head(datosc)

modelo=lm(cbind(Y1,Y2)~X1+X2,data=datosc)
summary(manova(modelo),test="Pillai")
summary(manova(modelo),test="Wilks")
summary(manova(modelo),test="Hotelling-Lawley")
summary(manova(modelo),test="Roy")

library(car)
Manova(modelo,type="II")
Manova(modelo,type="III")

#Para hacer la prueba en conjunto
#regresion multivariada
#Ho: las variables dependientes no dependen de las var predictoras
# H1: las variables de...       si dependen de las 
lh.out <- linearHypothesis(modelo, hypothesis.matrix = c("X1 = 0", "X2 = 0"))
lh.out

