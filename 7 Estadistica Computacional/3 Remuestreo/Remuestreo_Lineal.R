## SCRIPT REMUESTREO

# install.package("boot")
library(boot)
set.seed(42)
n <- 30
x<-runif(n,0,10)
y<-3+2*x+rnorm(n,0,2)
datos<-data.frame(x=x,y=y)

boot_fn<-function(data,indices){
  muestra<-data[indices,]
  modelo<-lm(y~x,data=muestra)
  return(coef(modelo)[2]) #beta1
}
#Bootstrap con 1000 repeticiones
resultado<-boot(data=datos,statistic=boot_fn,R=1000)
boot.ci(resultado,type=c("perc","bca"))##percentiles y vies(sesgo) 









