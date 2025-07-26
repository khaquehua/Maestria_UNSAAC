library(cluster)
library(fpc)
library(mclust)
library(flexmix)
library(prabclus)
library(diptest)
library(trimcluster)
library(plyr)
library(modeltools)
library(mvtnorm)
library(robustbase)
library(kernlab)
library(factoextra)

#-------------------------------------------------------#
# Ejemplo distritos                                     #
#-------------------------------------------------------#
library(foreign)
distritos=read.spss("distritos.sav",
                    #distritos=read.spss(file.choose(),
                    use.value.labels=TRUE, 
                    max.value.labels=Inf, 
                    to.data.frame=TRUE)

colnames(distritos) <- tolower(colnames(distritos)) # para poner en minuscula los nombres de columna
nombres=distritos[,1]
distritos=distritos[,-1]
rownames(distritos)=nombres # para asignar nombres a las filas.
head(distritos)

#-------------------------------------------------------#
#  Algoritmo Divisivo (DIANA)                           # tecnicas divisivas
#-------------------------------------------------------#

res.diana=diana(scale(distritos))
res.diana
plot(res.diana)

# Usando matriz de disimilaridad
diss.distritos=daisy(scale(distritos))
res=diana(diss.distritos)
plot(res)

# Determinando el n?mero ?ptimo de conglomerados
# Indice de Silueta
par(mfrow=c(1,3))
for(h in 2:4){
  conglomerados=cutree(res,h)
  plot(silhouette(conglomerados,diss.distritos))
}

# indice de silueta me aconseja que sean dos conglomerados

fviz_dend(res.diana, cex = 0.5,
          k = 2, 
          palette = "jco" 
)

#PERFILAMIENTO
# EN CONCLUSION VAMOS A SEPARAR EN 2 GRUPOS A NUESTRO CLUSTER
conglomerados=cutree(res,2)
DATANUEVA= cbind(distritos,cluster=conglomerados)

attach(DATANUEVA)
par(mfrow=c(2,4))
boxplot(ocu_vivi~cluster)
boxplot(pobpjov~cluster)
boxplot(sinelect~cluster)
boxplot(sinagua~cluster)
boxplot(pea1619~cluster)
boxplot(pocprin~cluster)
boxplot(peam15~cluster)
