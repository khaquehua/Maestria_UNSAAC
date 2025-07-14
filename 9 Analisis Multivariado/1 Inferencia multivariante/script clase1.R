library(readxl)
datos_A <- read_excel("datos1.xlsx")

# se puede visualizar la matriz de datos
View(datos_A)

# el vector de medias
colMeans(datos_A)

# la matriz de varianzas y cov
cov(datos_A)

# la matriz de correlaciones
cor(datos_A)

# graficos de correlaciones
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(datos_A, histogram = TRUE, method = "pearson")

library(psych)
corPlot(datos_A, cex = 2.5, main = "Matriz de correlación")

