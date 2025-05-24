## Script para el algoritmo de MH
set.seed(4606)
sigma <-2 #Parámetro de escala de la distribución Normal
n <- 10000 ## generar 100 iteraciones
x <- rep(0,n) #Creamos estados de la cadena
x
x[1] <- rnorm(1) # Si no se coloca los parametro de escala y forma entonces N(0,1)
for (i in 2: n){
  y <- x[i-1]+sigma*rnorm(1) # y es el parametro
  # propuesto, x[i] es el para¡metro a ser inferenciado
  u <- runif(1)
  ratio <- (dcauchy(y)*dnorm(x[i-1],y,sigma))/
    (dcauchy(x[i-1])*dnorm(y,x[i-1],sigma))
  alpha <- min(1,ratio)
  if(u<=alpha){x[i] <- y}
  else{x[i] <- x[i-1]}
}
y=x
#Visualmente vemos el comportamiento de la cadena (Ver si ya convergio)
#Ultimas 10,000 iteraciones
plot(seq(n-1000,n),y[(length(y)-1000):length(y)],
     type = "l",xlab='t',ylab=expression(y),main="Cadena")


#calentamiento o burn-in, eliminando las 100 primeras iteraciones
burn<-1000
y=y[burn:length(y)]
#Vemos independencia
acf(y)
library(MASS)
truehist(y)
curve(dcauchy(x),-30,30,add = TRUE, col="red")
