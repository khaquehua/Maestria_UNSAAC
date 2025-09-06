
datos = read.delim("clipboard", header = T)
head(datos)
datos$maquina= factor(datos$maquina)

# grafico de boxplot
attach(datos)
boxplot(volumen~maquina, col="gold")
library(Rcmdr)
plotMeans(volumen,maquina, error.bars = "conf.int")

# prueba de media independientes
# prueba de normalidad
shapiro.test(volumen[1:10]) #los datos prov de una distr normal
shapiro.test(volumen[11:20]) #los datos prov de una distr normal

# prueba de homogeneidad de varianzas
# Ho: sigma^2_1 = sigma^2_2 (son homogeneas)
# Ha: sigma^2_1 != sigma^2_2 (son heterogeneas)
bartlett.test(volumen~maquina)
# como pvalor >0.05, no se rechaza la Ho
# las varianzas se consideran similares
# homogeneidad entre las varianzas.

# prueba de comparacion de dos medias
# Ho: mu_1 = mu_2
# Ha: mu_1 != mu_2
t.test(volumen~maquina, alternative = "t", var.equal=T)
# como el pvalor = 0.4347 >0.05, no se rechaza la Ho
# en promedio el llenado (volumen) es similar para
# ambas maquinas 

mod1 = aov(volumen~maquina)
summary(mod1)

mod2 = lm(volumen~maquina)
summary(mod2)

# aleatorizar un dca
library(agricolae)
trt <- c("T1", "T2", "T3","T4")
repeticion <- c(3, 3, 3,3)
outdesign <- design.crd(trt,r=repeticion,seed=777,serie=0)
book1 <- outdesign$book
print(book1)














