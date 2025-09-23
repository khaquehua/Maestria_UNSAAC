Temperatura <- c(rep(c(-1,0,1), each = 2 * 3))
Humedad <- c(rep(rep(c(-1,0,1), each = 2), times = 3))

# Valores exactamente como en la tabla:
# Fila Humedad=50%: Frio(1.5,1.2), Ambiental(3.5,3.2), Caliente(4.0,4.2)
# Fila Humedad=70%: Frio(1.4,1.3), Ambiental(2.9,2.5), Caliente(3.8,3.4)
# Fila Humedad=90%: Frio(0.8,1.2), Ambiental(1.8,2.0), Caliente(2.7,3.0)

Fuerza <- c(
  # Humedad 50%
  1.5, 1.2,  3.5, 3.2,  4.0, 4.2,
  # Humedad 70%
  1.4, 1.3,  2.9, 2.5,  3.8, 3.4,
  # Humedad 90%
  0.8, 1.2,  1.8, 2.0,  2.7, 3.0
)

datos <- data.frame(Temperatura, Humedad, Fuerza)
head(datos)

# calculando los efectos del modelo y el anva para factores cuantitativos
mod = lm(Fuerza ~ Temperatura*Humedad+ I(Temperatura^2) +I(Humedad^2), data=datos)
summary(mod)
anova(mod)

# seria una analisis de varianza clasico considerando los factores cualitativos
anva = aov(Fuerza ~ factor(Temperatura)*factor(Humedad), data=datos)
summary(anva)

# Tabla de medias por celda y totales marginales
aggregate(Fuerza ~ Temperatura + Humedad, data = datos, mean)
# se observa que a medida que se aumenta la temnperatura, 
# la fuerza de adherencia del pegamente disminuye, sin considerar los niveles de humedad
aggregate(Fuerza ~ Temperatura, data = datos, mean)
# a medida que la temperatura aumenta, la fuerza disminuye
aggregate(Fuerza ~ Humedad, data = datos, mean)
# a medida que la humedad aumenta, la fuerza tambien aumenta

# vamos a verificar los supuestos 

mod1<-lm(Fuerza~Humedad+ Temperatura+ Humedad*Temperatura+I(Humedad^2))
summary(mod1)
# de manera grafica

par(mfrow = c(2,2))
plot(mod1)

# con hipotesis
# obteniendo los residuos estandarizados
ri = rstandard(mod1)
head(ri)
#prueba de normalidad
shapiro.test(ri) # los residuos siguen una distribucion normal
# prueba de homogeneidad
library(car)
ncvTest(mod1) # existe varianza constante (homogenedidad de varianzas)
library(lmtest)
lmtest::dwtest(mod1) # los errores son independientes


library(rsm)
# Ajustar el modelo para rsm
modelo_rsm <- rsm(Fuerza ~ SO(Humedad, Temperatura), data = datos)

# Superficie de respuesta para A y B, con C en nivel central (0)
par(mfrow = c(1, 2))

# Contorno
contour(modelo_rsm, ~ Humedad+ Temperatura, image = TRUE,
        main = "Contorno: fUERZA vs Humedad y temperatura")

# Perspectiva 3D
persp(modelo_rsm, ~ Humedad+ Temperatura, col = "lightblue",
      main = "Contorno: fUERZA vs Humedad y temperatura")










