#::::::::::::    SCRIPT KEVIN HAQUEHUA     :::::::::::::::::::::::::::::::::::::
# LIBRERIAS A UTILIZAR
library(readxl)
library(ggplot2)
library(agricolae)
library(car)
library(lmtest)
library(dplyr)
library(gmodels)

# LECTURA DE DATOS
data <- read_excel("11 Diseno Experimentos/Examen_1/data_floracion.xlsx")
attach(data) # Para tomar los datos
data$tratamiento  = as.factor(tratamiento) # convierte tratamiento en factor

# GRAFICO DE LOS TRATAMIENTOS
ggplot(data, aes(x = tratamiento, y = floracion, fill = tratamiento)) +
  geom_violin(alpha = 0.5) +
  theme(legend.position = "none")+
  geom_boxplot(width = 0.2)

# MODELO
mod.dca = lm(floracion ~ tratamiento) # modelo DCA
summary(aov(mod.dca)) # Análisis de varianza

# PRUEBA DE NORMALIDAD
residuales = residuals(mod.dca) # extrae residuales
histo      = hist(residuales, col="gold", xlab="residuales", main="Histograma de los residuales", ylab="Densidad",prob=1)
normal.freq(histo, frequency=3) # densidad
plot(mod.dca, which = 2)
shapiro.test(residuales) # Preuba de Shapiro

# HOMOCEDASTICIDAD
plot(mod.dca, which = 5)
plot(mod.dca, which = 1)
ncvTest(mod.dca)

# INDEPENDENCIA DE ERRORES
plot(residuals(mod.dca), pch = 18,
     type = "b", ylab = "residuales",
     xlab = "Índice", main="Residuales")
abline(h=0)
dwtest(mod.dca,alternative = c("two.sided")) # Prueba de Durbin Watson

# COEFICIENTE DE VARIACION
cv = cv.model(mod.dca)
cv

# MEDIA GENERAL
med_general = mean(floracion); med_general

# MEDIA Y EFECTOS DE LOS TRATAMIENTOS
med_trat = aggregate(floracion~tratamiento, FUN = mean)
med_trat%>% rename(Media = floracion) %>% mutate(Efecto = Media-med_general)

# CONTRASTES ORTOGONALES
con = c(0, -2, 1, 0, 0)
fit.contrast(mod.dca , "tratamiento", con)

con = c(0, 0.5, 0.5, -0.5, -0.5)
fit.contrast(mod.dca , "tratamiento", con)
