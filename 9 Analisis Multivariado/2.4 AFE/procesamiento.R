# procesamiento de la tesis de calidad de enseñanza

library(foreign)
datos=read.spss("dataTesis.sav", use.value.labels = T, to.data.frame = T)
View(datos)
#str(datos)

# No considerar la primera columna Id
datos1=datos
summary(datos1)

#str(datos1)
# eliminando registros con datos faltantes
datos1 = na.omit(datos1)

#########################################
# An?lisis Exploratorio con 7 variables #
#########################################

# Analisis descriptivo y Analisis de Correlacion
# library(psych)
# describe(datos1)
# cor(datos1)
# 
# corr.test(datos1)
# cor.plot(cor(datos1))

# Prueba de Esfericidad de Bartlett
library(psych)
cortest.bartlett(cor(datos1), n=nrow(datos1))

# Indicador Kaiser-Meyer-Olkinn KMO y MSA 
KMO(datos1)


# An?lisis Factorial con rotacion con funci?n principal
library(psych)
facto=principal(r=datos1,nfactors=8,rotate="varimax")
facto
facto$loadings

write.csv(facto$loadings,"factoinicial2.csv")
#write.csv(facto$loadings, "fatores.csv")
# Gr?fica de Matriz de Correlaci?n entre  Variables y Factores
# library(psych)
# cor.plot(cor(datos1),
#          main="Mapa de Calor", 
#          diag=TRUE,
#          show.legend = TRUE)       
# 
# library(GGally)
# ggpairs(datos1)

library(psych)
facto=principal(r=datos1,nfactors=5,rotate="varimax")
facto
facto$loadings

write.csv(facto$loadings,"datafinal.csv")

autovalores=c(16.503, 13.948, 11.594, 10.998, 5.143)
plot(autovalores,type="h")
abline(h=1)

plot(prcomp(datos1, scale = TRUE))
abline(h=1, col="red")
# write.csv(facto$loadings, "fatores2.csv")

biplot(prcomp(datos1, scale = FALSE))
abline(h = 0, v = 0, lty = 2, col = 8)

# calculo de la matriz de corrlaciones
correl=cor(datos1,use="pairwise.complete.obs") 
correl


scores <- as.matrix(datos1) %*% as.matrix(facto$loadings)

scores <- data.frame(scores)
head(scores)
summary(scores)

# modelo1=lm(scores$RC1~scores$RC2)
# summary(modelo1)

# entonces vamos hacer una transformacion manteniendo sus caracteristicas
Zscores<-scale(scores)
transScore <- Zscores*100+500
transScore <- data.frame(transScore)
hist(transScore$RC1)

# RECODIFICANDO 

#fACTOR1
transScore$RNC1[transScore$RC1<350] <-1
transScore$RNC1[transScore$RC1>=350 & transScore$RC1<450] <-2
transScore$RNC1[transScore$RC1>=450 & transScore$RC1<550] <-3
transScore$RNC1[transScore$RC1>=550 & transScore$RC1<650] <-4
transScore$RNC1[transScore$RC1>=650] <-5


# Etiquetar
transScore$RNC1 <- factor(transScore$RNC1, 
                             labels = c("Pesima", "Mala", "Regular",
                                        "Buena", "Excelente"))


fi=table(transScore$RNC1)
probabilidad=prop.table(table(transScore$RNC1))*100
cbind(fi,probabilidad)
barplot(prop.table(table(transScore$RNC1)), col = "darkBlue", xlab = "Factor1")

