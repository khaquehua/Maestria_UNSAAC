
# Caso 1

# install.packages("agricolae")
library(agricolae)
trt = c("Cepa 1","Cepa 2","Cepa 3","Cepa 4")  # nombre de los tratamientos
r   = rep(5,4) # n?mero de repeticiones por tratamiento
diseno1 = design.crd(trt,r) # ejecuta el croquis
diseno1$book # muestra el croquis

datos = read.table(file.choose(),T) # lectura de datos
datos # presentaci?n de datos
attach(datos)
cepa  = as.factor(cepa) # convierte cepa en factor
library(Rcmdr)
plotMeans(altura, cepa, error.bars="conf.int", 
          xlab = "cepas", ylab = "altura media",
          main="Intervalos de confianza")


# Definici?n del modelo

mod.dca = lm(altura ~ cepa) # modelo DCA


# An?lisis de varianza

summary(aov(mod.dca)) # An?lisis de varianza

# Comparaciones m?ltiples

# === #
# LSD #
# === #

LSD.1a = LSD.test(mod.dca,"cepa", p.adj="bonferroni", group = FALSE)
print(LSD.1a$comparison)

LSD.1b = LSD.test(mod.dca,"cepa", p.adj="bonferroni", group = TRUE)
print(LSD.1b$groups)
plot(LSD.1b)

# ===== #
# Tukey #
# ===== #

Tukey.1a  = HSD.test(mod.dca, "cepa", group=FALSE)  # usando agricolae
print(Tukey.1a$comparison)

Tukey.1b  = HSD.test(mod.dca, "cepa", group=TRUE)   # usando agricolae
print(Tukey.1b$groups)

TukeyHSD(aov(mod.dca), which = "cepa") # usando stats

plot(Tukey.1b)
plot(Tukey.1b, variation = "IQR")
plot(Tukey.1b, variation = "SD")

bar.group(Tukey.1b$groups, xlab="Cepas", ylab="Altura", horiz = FALSE, col = "darkgreen")
bar.group(Tukey.1b$groups, ylab="Cepas", xlab="Altura", horiz = TRUE, col = "darkgreen")

# ====== #
# Duncan #
# ====== #

Duncan.1a = duncan.test(mod.dca,"cepa", group = FALSE)
print(Duncan.1a$comparison)

Duncan.1b = duncan.test(mod.dca,"cepa", group = TRUE)
print(Duncan.1b$groups)
plot(Duncan.1b)


# ======= #
# Scheffe #
# ======= #

Scheffe.1a = scheffe.test(mod.dca,"cepa", group = FALSE)
print(Scheffe.1a$comparison)

Scheffe.1b = scheffe.test(mod.dca,"cepa", group = TRUE)
print(Scheffe.1b$groups)
plot(Scheffe.1b)


# ======= #
# Dunnett #
# ======= #

# install.packages("multcomp")
library(multcomp)
cont.dun  = contrMat(n=c(5,5,5,5), 
                     type = c("Dunnett"),
                     base=4)   # creaci?n del contraste Dunnett
Dunnett.a = glht(aov(altura ~ cepa), 
                 linfct=mcp(cepa=cont.dun),   
                 alternative="two.sided")     # ejecuci?n de la prueba Dunnett
summary(Dunnett.a) # resultados de la prueba Dunnett
plot(Dunnett.a)

recod = c("A"=4,"B"=1,"C"=2,"D"=3)  # recodificaci?n
Cepa  = factor(cepa,
               levels = recod, 
               labels = names(recod)) # creaci?n de la nueva variable Cepa en vez de cepa
Dunnett.b = glht(aov(altura ~ Cepa), 
                 linfct=mcp(Cepa="Dunnett"),
                 alternative="two.sided")     # ejecuci?n de la prueba Dunnett
summary(Dunnett.b) # resultados de la prueba Dunnett


# ====================== #
# Contrastes ortogonales #
# ====================== #

library(gmodels)
con = matrix(c(1, -1/2, -1/2, 0), 4, 1)
L   = t(con)
L
fit.contrast(aov(altura ~ cepa), "cepa", L)



# Coeficiente de variabilidad
cv = cv.model(mod.dca) # calcula el coeficiente de variabilidad (paquete agricolae)
cv

# Verificaci?n de supuestos


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

# ===================== #
# Homocedasticidad      #
# ===================== #

plot(mod.dca, which = 5) # ?La variabilidad es distinta entre los factores?

plot(mod.dca, which = 1) # ?la variabilidad se incrementa con la media de Y?

bartlett.test(altura ~ cepa) # prueba de Bartlett
library(Rcmdr)
leveneTest(altura ~ cepa) # prueba de Levene (solo usar si no se verifica normalidad)
library(car)
ncvTest(mod.dca) # prueba de Breusch - Pagan

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


# =======================================#
# Potencia de prueba y tama?o de muestra #
# =======================================#

#install.packages("daewr")
library(daewr)
rmin = 2 # menor n?mero de repeticiones
rmax = 10 # mayor n?mero de repeticiones
alpha = rep(0.05, rmax - rmin +1)
sigma = sigma(mod.dca)
nlev  = 3
nreps = rmin:rmax
Delta = 4 # la diferencia m?nima que se desea detectar
power = Fpower1(alpha,nlev,nreps,Delta,sigma)
power
