#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::         SEGUNDO EJERCICIO              :::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
datos.3 = read.table("11 Diseno Experimentos/4_DCA_2F/datos3.txt",T)
# Variables de estudio: porcentaje de reaccion
# Factores de estudio: A: Tipo de alcohol y B: base

# Lectura de datos
datos.3
# Presentaci?n de los datos
attach(datos.3) # Asignaci?n de variables

Alcohol = as.factor(Alcohol)
Base = as.factor(Base)

xtabs(Porcentaje ~ Base + Porcentaje) # Presentaci?n de la tabla de datos

interaction.plot(Alcohol, Base, Porcentaje, lwd = 2, col = 4, ylab = "Porcentaje medio de reacción")
# Aca se observa una interacción

# PRIMERO PRUEBA DE HIPOTESIS DE INTERACCION
# H0: {(\alpha \beta)}_{ij} = 0 (Indica que el efecto de interacción no es significativo)
# H1: {(\alpha \beta)}_{ij} != 0 (Indica que el efecto de interacción es significativo)

library(agricolae)
mod.ef3 = lm(Porcentaje ~ Alcohol * Base) # Construcci?n del modelo
summary(aov(mod.ef3)) # An?lisis de varianza

# Se tiene un Fvalue = 11.283 y un pvalor 0.01 el cual es menor al 0.1 por lo que
# se rechaza H0 como verdadera:
# CONCLUSION: A un nivel de significacion del 10% podemos afirmar que el efecto de
# la interacción entre los factores alcohol y base es significativo

# Coeficiente de variabilidad
cv.model(mod.ef3)

# Verificar supuestos Graficamente
par(mfrow = c(2,2))
plot(mod.ef3)

# Verificar supuestos con hipotesis
library(nortest)
library(car)

shapiro.test(residuals(mod.ef3)) # Prueba de normalidad de errores
ad.test(residuals(mod.ef3)) # Prueba de normalidad de errores

# Prueba de homocedasticidad (homogeneidad de varianzas)
library(car)
# H0: La varianza es constante en los residuos
# H0: La varianza no es constante en los residuos
ncvTest(mod.ef3) 

# Prueba de independencia de errores
library(lmtest)
# H0: Rho = 0 (Los errores son independientes) o (no existe autocorrelación de los residuos)
# Ha: Rho != 0 (Los errores no son independientes) o (existe autocorrelación de los residuos)
dwtest(mod.ef3,alternative = c("two.sided")) 
# Como el pvalor es mayor a 0.05 por lo que no se rechaza H0 indicando que los
# errores son independientes

#::::::::::::::       ANALISIS DE EFECTOS SIMPLES      ::::::::::::::::::::::
# Fijando el alcohol y comparando las bases
library(phia)
testInteractions(mod.ef3, fixed="Alcohol", across="Base")
aggregate(Porcentaje ~ Base*Alcohol, FUN = mean)

# Fijando las bases y comparando el alcohol
testInteractions(mod.ef3, fixed="Base", across="Alcohol")
aggregate(Porcentaje ~ Alcohol*Base, FUN = mean)
library(lsmeans)
lsmeans(mod.ef3, pairwise ~ Alcohol|Base)

