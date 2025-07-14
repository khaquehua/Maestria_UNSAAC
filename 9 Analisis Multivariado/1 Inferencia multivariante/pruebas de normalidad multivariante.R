
## pruebas para verificar normalidad multivariante.
library(MVN)
library(readxl)
datos <- read_excel("D:/0_POS GRADO UNSAAC/MULTIVARIANTE-JULIO 2025/Tasas.xlsx", 
                    range = "B1:D52")
View(datos)

# verificando normalidad univariada
# Ho:Los datos de mortalidad infantil siguen una distribucion normal
# Ha:Los datos de mortalidad infantil no siguen una distribucion normal
shapiro.test(datos$Mort.Infan)
# W = 0.934, p-value = 0.00711 <0.05
# se rechaza la Ho como verdadera.
# Los datos de mortalidad infantil no siguen una distribucion normal
shapiro.test(datos$Fertilidad)
shapiro.test(datos$No.Católicos)

library(nortest)
nortest::ad.test(datos$Mort.Infan)  # Anderson darling
nortest::cvm.test(datos$Mort.Infan) # cramer vonn mises

# Normalidad multivariada

# Ho:El vector siguen una distribucion normal p variante
# Ha:El vector No siguen una distribucion normal p variante

library(MVN)
Mardia = mvn(datos, mvnTest = "mardia")
Mardia$multivariateNormality
# para que un vector sea considerado normal p variante
# se requiere que tanto en asimetria como en kurtosis 
# se cumpla la normalidad.
# conclusion para nuestros datos no se cumplen los 
# supuestos de normalidad

# 2 Henze-Zirkler
HZ =mvn(datos, mvnTest = "hz") 
HZ$multivariateNormality

# 3 Royston
Roy =mvn(datos, mvnTest = "royston") 
Roy$multivariateNormality

# 4 shapiro wilk multivariante
library(mvnormtest)
mshapiro.test(t(datos))

# 5 Doornik-Hansen's test 
DH =mvn(datos, mvnTest = "dh") 
DH$multivariateNormality


