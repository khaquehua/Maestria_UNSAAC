#####################################################
#                                                   #
# AN?LISIS DE COMPONENTES PRINCIPALES               # 
                                                    #                     
#####################################################


#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

boxplot(datos.acp)
###############
#  Paquetes   #
###############

library(ade4)
library(FactoMineR)
library(psych)
library(car)
library(factoextra)
library(PerformanceAnalytics)
library(GGally)

####################
# Lectura de datos #
####################

library(foreign)
datos <-read.spss("PEA.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)
head(datos)
str(datos)

# No considerar la primera ni la columna once: Id y si ingres?

datos.acp <- datos[c(-15),c(-1)]
head(datos.acp)

str(datos.acp)

########################
# An?lisis Descriptivo #
########################

summary(datos.acp)

library(psych)
describe(datos.acp)

library(pastecs)
stat.desc(datos.acp)
stat.desc(datos.acp,basic=FALSE)

###########################
# An?lisis de Correlaci?n #
###########################

#---------------------------------
# Matriz de Variancia-Covariancia
cov(datos.acp)
options(digits = 3)
cov(datos.acp)

diag(cov(datos.acp))
sum(diag(cov(datos.acp)))


#-----------------------------
# Coeficientes de Correlaci?n 
cor(datos.acp)

# Prueba estad?stica
library(psych)
corr.test(datos.acp)

library(corrplot)
R=cor(datos.acp,method="pearson")
corrplot(R,sig.level=0.05,type="lower")
?corrplot

#----------------------------------------------
# Gr?ficos de Correlaci?n

# Primera forma
pairs(datos.acp)



# Segunda forma
library(PerformanceAnalytics)
chart.Correlation(datos.acp, histogram=TRUE, pch=20)

# Tercera forma - Mapas de Calor
library(psych)
cor.plot(cor(datos.acp),
         main="Mapa de Calor", 
         diag=TRUE,
         show.legend = TRUE)  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#  SUPUESTOS

# Supuesto de variables correlacionadas. Prueba de Esfericidad de Bartlett

library(psych)
library(rela)
options(scipen=0)
cortest.bartlett(cor(datos.acp),n=dim(datos.acp))

# Supuesto de normalidad multivariada: Prueba de Shapiro Wilk

library(mvnormtest)
mshapiro.test(t(datos.acp))



##################################################################
# 1. An?lisis de Componentes Principales usando la librer?a ade4 #
#                   Matriz de Correlaciones                      #
##################################################################

library(ade4)
acp <- dudi.pca(datos.acp,
                scannf=FALSE, scale=TRUE,
                nf=ncol(datos.acp))# Con scale se estandarizan las variables
summary(acp)
str(acp)
print(acp)

# Valores propios (autovalores)
acp$eig
sum(acp$eig)

inertia.dudi(acp)

# Vectores propios
acp$c1

# Correlaciones entre las variables y los componentes
acp$co

# Gr?fica de Valores propios - ScreePlot

# Primera forma
plot(acp$eig,type="b",pch=20,col="blue")
abline(h=1,lty=3,col="red")

library(factoextra)
fviz_eig(acp,geom="line")+
theme_grey()


# Segunda forma
library(factoextra)
eig.val <- get_eigenvalue(acp)
eig.val

barplot(eig.val[, 2], names.arg=1:nrow(eig.val), 
        main = "Autovalores",
        xlab = "Componentes Principales",
        ylab = "Porcentaje de variancias",
        col ="steelblue")
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type="b", pch=19, col = "red")

# Tercera forma
library(factoextra)
fviz_screeplot(acp, ncp=6)
fviz_eig(acp, addlabels=TRUE, hjust = -0.3)

fviz_eig(acp, addlabels=TRUE, hjust = -0.3,
              barfill="white", barcolor ="darkblue",
              linecolor ="red") + ylim(0,50) + theme_minimal()


# Gr?fica de Variables sobre el c?rculo de correlaciones

# Primera forma 
s.corcircle(acp$co,grid=FALSE)

# Segunda forma
fviz_pca_var(acp)

fviz_pca_var(acp, col.var="steelblue")+theme_minimal()


# Scores o Puntuaciones de cada individuo
acp$li[1:10,]

options(scipen=999)
cov(acp$li)
cor(acp$li)

describe(acp$li)

# Gr?fica de individuos sobre el primer plano de componentes

# Primera forma
s.label(acp$li,xax=1,yax=2,clabel=0.7,grid=FALSE,boxes=FALSE)

# Segunda forma 
fviz_pca_ind(acp)

# Gr?fica de individuos sobre los componentes 2 y 3
s.label(acp$li,xax=2,yax=3,clabel=0.7,grid=FALSE,boxes=FALSE)


# Gr?fica de individuos sobre el primer plano con biplot

# Primera forma
s.label(acp$li,clabel=0.7,grid=FALSE,boxes=FALSE)
s.corcircle(acp$co,grid=FALSE,add=TRUE,clabel=0.7)

# Segunda forma
fviz_pca_biplot(acp, repel = TRUE,
                col.var = "steelblue",
                col.ind = "black" )


# Grabar los datos y los resultados de los scores en un archivo CSV
salida.acp=cbind(datos.acp,acp$li[,c(1,2,3)])
salida.acp
str(salida.acp)
write.csv(salida.acp,"P.csv")

#OTRA ALTERNATIVA
library(FactoMineR)
acp=PCA(datos.acp,scale.unit = TRUE,ncp=9,graph = TRUE)
summary(acp)

#M?todo Paralelo para la retenci?n de componentes principales.
#Cuando hay subjetividad en el gr?fico de sedimentaci?n (Scree Plot) respecto al n?mero de CP a retener
#se puede recurrir al M?todo Paralelo

library(paran)
paran(datos.acp,iterations=5000,graph=TRUE,color=2)
#se confirma que se debe retener 3 CP.

#Test Estad?stico para retener m CP (H0:lamda(m+1)=lamda(m+2)=...=lamda(p)=0)
#Esta prueba tiene la limitaci?n de aconsejar la retenci?n de demasiadas Componentes Principales.
library(nFactors)
nBartlett(cor(datos.acp),N=541,alpha=0.01,cor=TRUE,details=TRUE)
#con este test se recomienda retener 7 u 8 CP.
