datos.2 = read.table("11 Diseno Experimentos/4_DCA_2F/datos2.txt",T)
# Lectura de datos
datos.2
# Presentaci?n de los datos
attach(datos.2) # Asignaci?n de variables
xtabs(Altura ~ Envase + Especie) # Presentaci?n de la tabla de datos

interaction.plot(Envase, Especie, Altura, lwd = 2, col = 4, ylab = "Rendimiento medio")

# PRIMERO PRUEBA DE HIPOTESIS DE INTERACCION
# H0: {(\alpha \beta)}_{ij} = 0 (Indica que el efecto de interacción no es significativo)
# H1: {(\alpha \beta)}_{ij} != 0 (Indica que el efecto de interacción es significativo)

library(agricolae)
mod.ef2 = lm(Altura ~ Envase * Especie) # Construcci?n del modelo
summary(aov(mod.ef2)) # An?lisis de varianza

# Se tiene un Fvalue = 0.875 y un pvalor 0.434 el cual es mayor al 0.05 por lo que no
# se rechaza H0 como verdadera:
# CONCLUSION: A un nivel de significacion del 5% podemos afirmar que el efecto de
# la interacción entre los factores recipiente y especie no es significativo

# Coeficiente de variabilidad
cv.model(mod.ef2)

# Verificar supuestos Graficamente
par(mfrow = c(2,2))
plot(mod.ef2)

# Verificar supuestos con hipotesis
library(nortest)
library(car)

shapiro.test(residuals(mod.ef2)) # Prueba de normalidad de errores
ad.test(residuals(mod.ef2)) # Prueba de normalidad de errores

# Prueba de homocedasticidad (homogeneidad de varianzas)
library(car)
# H0: La varianza es constante en los residuos
# H0: La varianza no es constante en los residuos
ncvTest(mod.ef2) 

# Prueba de independencia de errores
library(lmtest)
# H0: Rho = 0 (Los errores son independientes)
# Ha: Rho != 0 (Los errores no son independientes)
dwtest(mod.ef2,alternative = c("two.sided")) 
# Como el pvalor es mayor a 0.05 por lo que no se rechaza H0 indicando que los
# errores son independientes

#::::::::::::  APLICAR TRANFORMACION DE DATOS  :::::::::::::::::::::
# Gr?fico de Box Cox para elegir lambd
par(mfrow = c(1,1))
library(MASS)
boxcox(mod.ef2, plotit=TRUE, lambda = seq(-2, 10, 1/10))
boxcox(mod.ef2, plotit=FALSE, lambda = seq(-2, 10, 1/10))

# Realizamos la tranformacion de boxcox
library(forecast)
Altura2 <- BoxCox(Altura, 4.9)

datos.2$Altura2 = Altura2
mod.ef2b = lm(Altura2 ~ Envase * Especie) # Construcci?n del modelo
summary(aov(mod.ef2b))

cv.model(mod.ef2b) # Esta menos del 30% pasable

# AHORA VERIFICAMOS LOS SUPUESTOS
# Prueba de normalidad de errores
shapiro.test(residuals(mod.ef2b)) 

# Prueba de homocedasticidad (homogeneidad de varianzas)
ncvTest(mod.ef2b) 
# Ahora si se tiene varianza constante

# Prueba de independencia de errores
dwtest(mod.ef2b,alternative = c("two.sided")) 
# Ahora si hay homogeneidad de varianzas por lo que cumple los supuestos

# :::::::::::::::   COMPARACION MULTIPLE   ::::::::::::::::::::::::::::::::::
# :::::::::::: DE EFECTOS PRINCIPALES PARA EL FACTOR ENVASE O RECIPIENTE :::
# usa el nombre real de tu data.frame, aquí supongo df
datos.2$Envase <- factor(datos.2$Envase)
datos.2$Especie <- factor(datos.2$Especie)
# ajusta el modelo indicando 'data' explícitamente
mod <- aov(Altura2 ~ Envase + Especie, data = datos.2)
library(multcomp)
comp.2a <- glht(mod, linfct = mcp(Envase = "Tukey"))
summary(comp.2a); plot(comp.2a)

# Hallar la media por grupos
tapply(Altura2, Envase, mean)

# Recomiendas R1 y R2
# R1 = a
# R2 = a
# R3 = b

comp.2b <- glht(mod, linfct = mcp(Especie = "Tukey"))
summary(comp.2b); plot(comp.2b)
