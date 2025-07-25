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

#########################################################
#  Distancias                                           #
#########################################################
set.seed(666)
x =matrix(rnorm(20), nrow=5)
dist(x) # distancia euclideana
dist(x, method= "manhattan",diag = TRUE)
dist(x, method= "maximum",upper = FALSE)
?dist

library(cluster)

#x =matrix(rnorm(20), nrow=5)
daisy(x)
x = cbind(rnorm(10),sample(1:3,10,replace=T))
x<-as.data.frame(x)
x[,2]<-as.factor(x[,2])
daisy(x)

#=======================================================#
#  Metodos Particionales                                #
#=======================================================#

#########################################################
#  K-Medias o Kmeans                                          #
#########################################################

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


res<-kmeans(scale(distritos),3)
res

#------------------------------------#
# Determinar numero de conglomerados #
#------------------------------------#
kmeansruns(scale(distritos),criterion="ch") # calinski harabats
kmeansruns(scale(distritos),criterion="asw") # silueta

# por recomendacion de ambos criterios, vamos a trabajar con solo 2 cluster

res=kmeans(scale(distritos),2)
plotcluster(distritos,res$cluster)

clusplot(distritos,res$cluster, color = TRUE,
         shade = TRUE, labels =2,lines=0,
         main ="Gráfico de Conglomerados")

res$cluster
#-----------------------------------------#
# Perfilado y caracterización de clusters #
#-----------------------------------------#

# Adicionar los cluster a la base de datos
distritos.new<-cbind(distritos,res$cluster)
colnames(distritos.new)<-c(colnames(distritos.new[,-length(distritos.new)]), "cluster.km")
View(distritos.new)

# Tabla de medias
med <- aggregate(x = distritos.new[,1:7],by = list(distritos.new$cluster.km),FUN = mean)
med

# Describir variables
par(mfrow=c(2,4))
for (i in 1:length(distritos.new[,1:7])) {
  boxplot(distritos.new[,i]~distritos.new$cluster.km, main=names(distritos.new[i]), type="l")
}
par(mfrow=c(1,1))

boxplot(distritos.new$ocu_vivi~distritos.new$cluster.km)

write.csv(distritos.new, "datoskm2.csv")
