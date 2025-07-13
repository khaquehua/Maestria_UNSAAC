## Pruebas para verificar normalidad multivariante
library(MVN)
library(readxl)
datos <- read_excel("9 Analisis Multivariado/Inferencia multivariante/Tasas.xlsx")
datos <- datos[,-1]
View(datos)

# Verificando normalidad univariada
shapiro.test(datos$Mort.Infan)
# Se rechaza H0 por lo que los datos de mortalidad no siguen la dis normal

# NORMALIDAD MULTIVARIADA
# H0: El vector sigue una distribución normal p variante
# H1: El vector no sigue una distribución normal p variante

library(MVN)
# Mardia
Mardia <- mvn(datos, mvn_test = "mardia")
Mardia$multivariate_normality

# Para que un vector sea considerado normal p - variante
# se requiere que tanto en asimetria como en kurtosis se cumpla la normalidad
# se cumpla la normalidad.
# Conclusión para nuestros datos, no se cumplen los supuestos de normalidad

# 2 Henze-Zikler
HZ <- mvn(datos, mvn_test = "hz")
HZ$multivariate_normality

# 3 Royston
Roy <- mvn(datos, mvn_test = "royston")
Roy$multivariate_normality

# 4 Shapiro Wilk multivariado
library(mvnormtest)
mshapiro.test(t(datos))

# 5 DH
DH <- mvn(datos, mvn_test = "doornik_hansen")
DH$multivariate_normality
