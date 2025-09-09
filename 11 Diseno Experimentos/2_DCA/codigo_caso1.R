
# Caso 1

# install.packages("agricolae")
library(agricolae)
trt = c("tratamiento 1","tratamiento 2","tratamiento 3","control")  # nombre de los tratamientos
r   = rep(5,4) # n?mero de repeticiones por tratamiento
diseno1 = design.crd(trt,r) # ejecuta el croquis
diseno1$book # muestra el croquis

library(readxl)
datos = read_xlsx("11 Diseno Experimentos/2_DCA/data_caso1.xlsx") # lectura de datos
datos # presentaci?n de datos
attach(datos)

datos$tratamiento  = as.factor(tratamiento) # convierte tratamiento en factor

library(Rcmdr)
plotMeans(altura, tratamiento, error.bars="conf.int", 
          xlab = "tratamientos", ylab = "altura media",
          main="Intervalos de confianza")
library(ggplot2)
ggplot(datos, aes(x = tratamiento, y = altura, fill = tratamiento)) +
  geom_violin(alpha = 0.5) +
  theme(legend.position = "none")+
  geom_boxplot(width = 0.2)

ggplot(datos)+
  aes(x=tratamiento,y=altura,fill=tratamiento)+
  geom_violin(alpha=0.5)+
  labs(title="Diagrama de violin",x="Salario Base",y="")+
  guides(fill="none")+
  theme_bw()

# calculo de los efectos de los tratamientos
med_general = mean(altura); med_general

library(dplyr)
med_trat = aggregate(altura~tratamiento, FUN = mean)
med_trat%>% rename(Media = altura) %>% mutate(Efecto = Media-med_general)


# Definición del modelo
mod.dca = lm(altura ~ tratamiento) # modelo DCA
# An?lisis de varianza
summary(aov(mod.dca)) # An?lisis de varianza
# existe evidencia estadistica suficiente para rechazar la hipotesis nula,
# por tanto se pude afirmar que existe una altura promedio de planta que difiere
# de otro, producto de alguna cepa o control.


# Comparaciones m?ltiples

# === #
# LSD #
# === #

LSD.1a = LSD.test(mod.dca,"tratamiento", p.adj="bonferroni", group = FALSE)
print(LSD.1a$comparison)

LSD.1b = LSD.test(mod.dca,"tratamiento", p.adj="bonferroni", group = TRUE)
print(LSD.1b$groups)
plot(LSD.1b)

# se puede observar de los resultados que tanto la cepa 3 y 1 son las mas
# agresivas y no permiten el normal crecimiento de las plantas.

# ===== #
# Tukey #
# ===== #

Tukey.1a  = HSD.test(mod.dca, "tratamiento", group=FALSE)  # usando agricolae
print(Tukey.1a$comparison)

Tukey.1b  = HSD.test(mod.dca, "tratamiento", group=TRUE)   # usando agricolae
print(Tukey.1b$groups)

TukeyHSD(aov(mod.dca), which = "tratamiento") # usando stats

plot(Tukey.1b)
plot(Tukey.1b, variation = "IQR")
plot(Tukey.1b, variation = "SD")

bar.group(Tukey.1b$groups, xlab="tratamientos", ylab="Altura", horiz = FALSE, col = "darkgreen")
bar.group(Tukey.1b$groups, ylab="tratamientos", xlab="Altura", horiz = TRUE, col = "darkgreen")


# ====== #
# Duncan #
# ====== #

Duncan.1a = duncan.test(mod.dca,"tratamiento", group = FALSE)
print(Duncan.1a$comparison)

Duncan.1b = duncan.test(mod.dca,"tratamiento", group = TRUE)
print(Duncan.1b$groups)
plot(Duncan.1b)


# ======= #
# Scheffe #
# ======= #

Scheffe.1a = scheffe.test(mod.dca,"tratamiento", group = FALSE)
print(Scheffe.1a$comparison)

Scheffe.1b = scheffe.test(mod.dca,"tratamiento", group = TRUE)
print(Scheffe.1b$groups)
plot(Scheffe.1b)


# ======= #
# Dunnett #
# ======= #

# install.packages("multcomp")
library(multcomp)
attach(datos)
cont.dun  = contrMat(n=c(5,5,5,5), 
                     type = c("Dunnett"),
                     base=4)   # creaci?n del contraste Dunnett
Dunnett.a = glht(aov(altura ~ tratamiento), 
                 linfct=mcp(tratamiento = cont.dun),   
                 alternative="two.sided")     # ejecuci?n de la prueba Dunnett
summary(Dunnett.a) # resultados de la prueba Dunnett
plot(Dunnett.a)


# recod = c("A"=4,"B"=1,"C"=2,"D"=3)  # recodificaci?n
# tratamiento  = factor(datos$tratamiento,
#                levels = recod, 
#                labels = names(recod)) # creaci?n de la nueva variable tratamiento en vez de tratamiento
# Dunnett.b = glht(aov(altura ~ datos$tratamiento), 
#                  linfct=mcp(tratamiento=cont.dun),
#                  alternative="two.sided")     # ejecuci?n de la prueba Dunnett
# summary(Dunnett.b) # resultados de la prueba Dunnett


# ====================== #
# Contrastes ortogonales #
# ====================== #

library(gmodels)
con = matrix(c(1, -1/2, -1/2, 0), 4, 1)
L   = t(con)
L
fit.contrast(aov(altura ~ tratamiento), "tratamiento", L)



# Coeficiente de variabilidad
cv = cv.model(mod.dca) # calcula el coeficiente de variabilidad (paquete agricolae)
cv

sigma(mod.dca)
desv = sqrt(5.38)
med_general
coefvar = (sigma(mod.dca)/med_general)*100
coefvar

# Verificación de supuestos
# ===================== #
# Normalidad de errores #
# ===================== #

residuales = residuals(mod.dca) # extrae residuales
histo      = hist(residuales, col="gold", xlab="residuales", main="Histograma de los residuales", ylab="Densidad",prob=1)
normal.freq(histo, frequency=3) # a?ade densidad

plot(mod.dca, which = 2)

shapiro.test(residuales) # prueba de Shapiro - Wilk 

library(nortest)
ad.test(residuales) # prueba de Anderson - Darling
nortest::lillie.test(residuales)
nortest::sf.test(residuales)
# tecnica del voting

res2 = rstudent(mod.dca)
res2
histo      = hist(res2, col="gold", xlab="residuales", main="Histograma de los residuales", ylab="Densidad",prob=1)
normal.freq(histo, frequency=3) # a?ade densidad

# Utilizando los residuos estudentizados
shapiro.test(res2)
library(nortest)
ad.test(res2) # prueba de Anderson - Darling
nortest::lillie.test(res2)
nortest::sf.test(res2)


# ===================== #
# Homocedasticidad      #
# ===================== #

plot(mod.dca, which = 5) # ?La variabilidad es distinta entre los factores?

plot(mod.dca, which = 1) # ?la variabilidad se incrementa con la media de Y?

# sobre la variable de respuesta
bartlett.test(altura ~ tratamiento) # prueba de Bartlett
library(Rcmdr)
leveneTest(altura ~ tratamiento) # prueba de Levene (solo usar si no se verifica normalidad)
library(car)
# sobre los residuos (varianza constante)
ncvTest(mod.dca) # prueba de Breusch - Pagan
# como el pvalor >0.05, se afirma que la
# varianza es constante en los residuos.

# ========================= #
# Independencia de errores  #
# ========================= #

# install.packages("lmtest")

plot(residuals(mod.dca), pch = 18,
     type = "b", ylab = "residuales",
     xlab = "?ndice", main="Residuales")

abline(h=0)
library(lmtest)
dwtest(mod.dca,alternative = c("two.sided")) # prueba de Durbin - Watson
# ho: No estan autocorrelacionados (1er orden) rho=0
# ha: Estan autocorrelacionados (1er orden) rho !=0

# =======================================#
# Potencia de prueba y tamaño de muestra #
# =======================================#

#install.packages("daewr")
library(daewr)
rmin = 2 # menor n?mero de repeticiones
rmax = 10 # mayor n?mero de repeticiones
alpha = rep(0.05, rmax - rmin +1)
sigma = sigma(mod.dca) # sqrt(cme)
nlev  = 3
nreps = rmin:rmax
Delta = 5 # la diferencia m?nima que se desea detectar
power = Fpower1(alpha,nlev,nreps,Delta,sigma)
power
