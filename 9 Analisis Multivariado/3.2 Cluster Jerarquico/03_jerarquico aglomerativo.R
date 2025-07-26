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
#  Cluster Jerárquico Aglomerativo                      #
#-------------------------------------------------------#
# Usando el enlace de Ward
res.hc=hclust(dist(scale(distritos)),method="ward.D")  #la matriz de disimilaridades = distancias  #puedes usar una base de datos con variables cualis y cuantis esto no se podria hacer en el de k medias
plot(res.hc)
?hclust

# Cortando el dendrograma considerando cuatro conglomerados
(res.hc4=cutree(res.hc, k=4))
x11()
fviz_dend(res.hc, cex = 0.6, k = 4, palette = "jco")


# Cortando el dendrograma considerando una altura de 8
(res.hc4=cutree(res.hc, h=40))
x11()
fviz_dend(res.hc, cex = 0.6, h=40 , palette = "jco")
# dar justamente en la altura horizontal, si no no funca



#Clustering jerarquico aglomerativo usando Agnes
library(cluster)

# Usando el enlace simple
res.hc.s=hclust(dist(scale(distritos)),method="single")
plot(res.hc.s)

res.hc.s=hclust(dist(scale(distritos)),method="complete")
plot(res.hc.s)

res.hc.s=hclust(dist(scale(distritos)),method="average")
plot(res.hc.s)

res.hc.s=hclust(dist(scale(distritos)),method="centroid")
plot(res.hc.s)

#-------------------------------------------------------#
#  AGNES  aglomerative nesting                                              # tecnica aglomerativo, ventaja nos da un coeficeinte de aglomeracion 
#-------------------------------------------------------#
?agnes
res.agnes.single = agnes(scale(distritos), method="single")
res.agnes.single
plot(res.agnes.single) #0.6 si este valor tiende a 1 el dendrogrma ayuda  a diferenciar mejor el grupo

res.agnes.ward=agnes(scale(distritos),method="ward")
res.agnes.ward
plot(res.agnes.ward) #0.92 tiende a uno es una manera eficiente de agrupar en el dendrograma

help("agnes")

# Usando matriz de disimilaridad
diss.distritos=daisy(scale(distritos))
res.agnes.ward2 =agnes(diss.distritos,method="ward")
plot(res.agnes.ward2)

# Determinando el número ?ptimo de conglomerados
# Indice de Silueta
par(mfrow=c(1,3))
for(h in 2:4){
  conglomerados=cutree(res.agnes.ward2,k=h)
  plot(silhouette(conglomerados,diss.distritos))
}
par(mfrow=c(1,1))
x11()
fviz_dend(res.agnes.ward2, cex = 0.7,
          k = 2, 
          palette = "jco" 
)
# nuevamente uso 2 conglomerados por que es el que esta maximizando el indice de la silueta


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ #

# En conclusión se utilizara solo 2 clusters
# Usando el enlace de Ward
res.hc=hclust(dist(scale(distritos)),method="ward.D")  #la matriz de disimilaridades = distancias  #puedes usar una base de datos con variables cualis y cuantis esto no se podria hacer en el de k medias
plot(res.hc)
# Cortando el dendrograma considerando cuatro conglomerados
(res.final=cutree(res.hc, k=2))
