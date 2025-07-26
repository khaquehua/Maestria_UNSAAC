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

#-------------------------------------------------------#
# Ejemplo distritos                                     #
#-------------------------------------------------------#
library(foreign)
distritos=read.spss("9 Analisis Multivariado/3.1 Cluster/distritos.sav",
                    #distritos=read.spss(file.choose(),
                    use.value.labels=TRUE, 
                    max.value.labels=Inf, 
                    to.data.frame=TRUE)

colnames(distritos) <- tolower(colnames(distritos)) # para poner en minuscula los nombres de columna
nombres=distritos[,1]
distritos=distritos[,-1]
rownames(distritos)=nombres # para asignar nombres a las filas.
head(distritos)


#########################################################
#  PAM                                                  #
#########################################################
# Para ver la funcion (pam)

res=pam(scale(distritos),3)  # normalizacion Z 
res

plot(res)

asw<-numeric()
for(h in 2:10){
  res<-pam(scale(distritos),h)
  asw[h-1]<-res$silinfo$avg.width
}
plot(2:10,asw,type="b",xlab="k",ylab="ASW")

par(mfrow=c(1,3))
for(h in 2:4){
  res=pam(scale(distritos),h)
  plot(res,which.plots=2)
}
# nos quedamos con 2 clusters debido a que 
# el promedio de silueta es mayor con 2 clusters


pamk(scale(distritos),criterion="asw") # coeficientes promedio de silueta
pamk(scale(distritos),criterion="ch") # CH CALINSKI HARABATZ
# pamk(scale(distritos),criterion="multiasw") # usar este criterio cuando la data es grande


res=pam(scale(distritos),2)
plotcluster(distritos,res$clustering)


clusplot(distritos,res$clustering, color = TRUE,
         shade = TRUE, labels =2,lines=0,
         main ="Gráfico de Conglomerados")

#PERFILAMIENTO
DATANUEVA= cbind(distritos,clu=res$clustering)

attach(DATANUEVA)
par(mfrow=c(2,4))
boxplot(ocu_vivi~clu)
boxplot(pobpjov~clu)
boxplot(sinelect~clu)
boxplot(sinagua~clu)
boxplot(pea1619~clu)
boxplot(pocprin~clu)
boxplot(peam15~clu)

write.csv(DATANUEVA, "DATOSNUEVOS.csv")
