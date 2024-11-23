#------------------------------------------------------------
#       Análisis de caso - Correlación
#------------------------------------------------------------

#llamamos al conjunto de datos
library(readxl)
data1 <- read_excel("data1.xlsx")
View(data1)

#realizamos el diagrama de dispersión
library(ggplot2)
ggplot(data1, aes(x=Edad, y=Tiempo)) + geom_point() + ggtitle("Gráfica 2: Diagrama de Dispersión") + xlab("Edad") + ylab("Tiempo de servicio") + geom_smooth(method=lm)

plot(data1, xlab = "X = Tiempo de servicio", ylab = "Y = Edad", xlim = c(25, 50), ylim = c(0, 25), pch = 19, col = "blue")

#calculamos la covarianza
covarianza= cov(data1)
covarianza

#calculamos el coeficiente de correlación de Pearson
r=cor(data1)
r

library(psych)
psych::corr.test(data1, use = "complete")

#Prueba para el coeficiente de correlación

cor.test(data1$Edad,data1$Tiempo, method = c("pearson"), conf.level = 0.95)

#------------------------------------------------------------
#       Análisis de caso - Regresión Lineal
#------------------------------------------------------------
# ( y ~ x )
modeloR <- lm( Tiempo ~ Edad, data = data1)
modeloR

# para ver el resumen del modelo
summary( modeloR )

plot(data1$Edad, data1$Tiempo, xlab = "X = Tiempo de servicio", ylab = "Y = Edad", xlim = c(25, 50), ylim = c(0, 25), pch = 19, col = "blue")
#abline(a= 22.046, b= 1.066,col = "red" ) #añadir la recta de regresión


#ANOVA para el modelo
anova(modeloR)

#--------------------------------------------------------------------------------------------------------------
#Diagnóstico del modelo. Normalidad de los residuos, homogeneidad de varianzas e incorrelación de los residuos
#--------------------------------------------------------------------------------------------------------------

# valores ajustados
data1$fitted.modeloR <- fitted( modeloR ) 

# residuos
data1$dresiduals.modeloR <- residuals( modeloR ) 

# residuos estudentizados
data1$rstudent.modeloR <- rstudent( modeloR ) 

#Normalidad de los residuos
shapiro.test( data1$rstudent.modeloR )

#Homogeneidad de varianzas - Comprobar la homocedasticidad para los residuos estudentizados del modelo ajustado
library(lmtest)
bptest( modeloR )

#Incorrelación de los residuos

dwtest( Edad ~ Tiempo, 
        alternative = "two.sided", 
        data = data1)
#on un p-value = 0.3944, mayor de 0.05, no podemos rechazar la hipótesis nula. Por lo tanto suponemos incorrelación para los residuos estudentizados del modelo ajustado.

#------------------------------------------------------------
#       Análisis de caso de estudio 2 - Correlación
#------------------------------------------------------------

#llamamos al conjunto de datos
library(readxl)
data2 <- read_excel("data2.xlsx")
View(data2)

#realizamos el diagrama de dispersión
library(ggplot2)
ggplot(data2, aes(x=X, y=Y)) + geom_point() + ggtitle("Gráfica 2: Diagrama de Dispersión") + xlab("Edad (años)") + ylab("Longitud ala (cm)") + geom_smooth(method=lm)

plot(data2, xlab = "X = Edad (años)", ylab = "Y = Longitud ala (cm)", xlim = c(0, 20), ylim = c(1, 6), pch = 19, col = "blue")

#calculamos la covarianza
covarianza= cov(data2)
covarianza

#calculamos el coeficiente de correlación de Pearson
r=cor(data2)
r

library(psych)
psych::corr.test(data2, use = "complete")

#Prueba para el coeficiente de correlación

cor.test(data2$X,data2$Y, method = c("pearson"), conf.level = 0.95)

#------------------------------------------------------------
#       Análisis de caso - Regresión Lineal
#------------------------------------------------------------
# ( y ~ x )
modeloR <- lm( Y ~ X, data = data2)
modeloR

# para ver el resumen del modelo
summary( modeloR )

plot(data2$X, data2$Y, xlab = "X = Edad (años)", ylab = "Y = Longitud ala (cm)", xlim = c(0, 20), ylim = c(1, 6), pch = 19, col = "blue")
abline(modeloR, col = 2, lwd = 3) #añadir la recta de regresión


#ANOVA para el modelo
anova(modeloR)

#--------------------------------------------------------------------------------------------------------------
#Diagnóstico del modelo. Normalidad de los residuos, homogeneidad de varianzas e incorrelación de los residuos
#--------------------------------------------------------------------------------------------------------------

# valores ajustados
data2$fitted.modeloR <- fitted( modeloR ) 

# residuos
data2$dresiduals.modeloR <- residuals( modeloR ) 

# residuos estudentizados
data2$rstudent.modeloR <- rstudent( modeloR ) 

#Normalidad de los residuos
shapiro.test( data2$rstudent.modeloR )

#Homogeneidad de varianzas - Comprobar la homocedasticidad para los residuos estudentizados del modelo ajustado
library(lmtest)
bptest( modeloR )

#Incorrelación de los residuos

dwtest( X ~ Y, 
        alternative = "two.sided", 
        data = data2)
