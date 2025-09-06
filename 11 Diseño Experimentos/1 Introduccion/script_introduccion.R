library(readxl)
datos <- read_excel("11 Diseño Experimentos/1 Introduccion/data1.xlsx")

datos$maquina <- factor(datos$maquina)

# grafico de boxplot
attach(datos)
boxplot(volumen~maquina,col = "gold")

# Si existe diferencia entre esos dos graficos? La diferencia entre una y otra
# se observa que no hay diferencias significativa

library(Rcmdr)
