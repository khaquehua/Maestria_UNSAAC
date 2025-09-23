# EJEMPLO DE GUTIERREZ 2^K=3

datos = read.table("DATA2K3.txt", T)

datos$AB = datos$A*datos$B
datos$AC = datos$A*datos$C
datos$BC = datos$B*datos$C
datos$ABC = datos$A*datos$B*datos$C
attach(datos)

ypA<-tapply(Y,A,mean)
ypA
Aefe<-ypA[2]-ypA[1]
Aefe

ypB<-tapply(Y,B,mean)
ypB
Befe<-ypB[2]-ypB[1]
Befe
# 
# ypAB<-tapply(y,AB,mean)
# 
# AB<-ypAB[2]-ypAB[1]
# AB

# ANOVA EN EL DISEÑO 2K=3

mod<-lm(Y~A+B+C+A*B +A*C+B*C + A*B*C, data = datos)
summary(mod)
# Otra forma de encontrar los efectos estimados

betaest<-coefficients(mod)
efectosest<-2*betaest[2:8]
data.frame(efectosest)

# sumas de cuadrados y ANVA
anva<-aov(mod)
summary(anva)

# PARA VERIFICAR SUPUESTOS 
mod1<-lm(Y~A+B+C+A*C)
summary(mod1)
CME<-deviance(mod1)/df.residual(mod1)
beta1<-coefficients(mod1)
I<-rep(1,length(Y))
X<-cbind(I,A,B,C,AC)
yest<-X%*%beta1
e<- Y-yest
e

H<-X%*%solve(t(X)%*%X)%*%t(X)
ri<-e/sqrt(CME*(1-diag(H)))
ri

par(mfrow=c(2,2))
plot(mod1)

ri<-rstandard(mod1)
shapiro.test(ri)

library(nortest)
ad.test(ri)

# VARIANZA CONSTANTE-- BREUSCH PAGAN
library(car)
ncvTest(mod1)

# GRAFICO DE LA SUPERFICIE DE RESPUESTA
mod<-function(x1,x2){0.0152-0.00975*x1-0.003525*x2}
x1<-seq(-3,-1,0.05)
x2<-seq(60,98,2)
z<-outer(x1,x2,mod)
par(mfrow=c(1,1))
persp(x1,x2,z,theta=-40,phi=30,ticktype="detailed",
      xlab="factorA",
      ylab="factorB", 
      zlab="y")


