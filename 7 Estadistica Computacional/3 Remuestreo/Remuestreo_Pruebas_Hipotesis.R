## SCRIPT REMUESTREO
# Bootstrap para la diferencia de medias de dos grupos A y B

set.seed(123)
grupo1<-rnorm(20,mean=5,sd=1)
grupo2<-rnorm(20,mean=6,sd=1)
datos<-data.frame(
  valor=c(grupo1,grupo2),
  grupo=rep(c("A","B"),each=20)
)

#Funcion para bootstrap de diferencia de medias 
diff_means<-function(data,indices){
  muestra<-data[indices,]
  mean_A<-mean(muestra$valor[muestra$grupo=="A"])
  mean_B<-mean(muestra$valor[muestra$grupo=="B"])
  return(mean_B - mean_A)
}

library(boot)
resultado_diff<-boot(data=datos,statistic=diff_means,R=1000)
boot.ci(resultado_diff,type=c("perc","bca"))
#histograma de las medias remuestreadas

hist(resultado_diff$t,breaks=30,
     main="Distribucion Bootstrap: Diferencia de Medias",
     col="skyblue",xlab="Diferencia B-A")








