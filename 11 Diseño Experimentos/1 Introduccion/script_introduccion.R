library(readxl)
datos <- read_excel("11 Diseño Experimentos/1 Introduccion/data1.xlsx")

datos$maquina <- factor(datos$maquina)

# grafico de boxplot
attach(datos)
boxplot(volumen~maquina,col = "gold")

# Si existe diferencia entre esos dos graficos? La diferencia entre una y otra
# se observa que no hay diferencias significativa

library(Rcmdr)
plotMeans(volumen, maquina, error.bars = "conf.int")

# Prueba de medias independintes
# Prueba de normalidad
shapiro.test(volumen[1:10]) # Los datos provienen de una dist. normal
shapiro.test(volumen[11:20]) # Los datos provienen de una dist. normal

bartlett.test(volumen~maquina)

# Prueba de comparacion de dos medias
# H0: mu_1 = mu_2
# H0: mu_1 != mu_2

t.test(volumen~maquina, alternative = "t", var.equal = T)

# Como el pvalor = 0.4347 > 0.05, no se rechaza H0 
# En promedio el llenado (volumen) es similar para
# ambas maquinas

# Aleatorizar un diseño completo al azar: Caso cuyes
trt <- c("T1","T2","T3","T4")
repeticion <- c(3,3,3,3)