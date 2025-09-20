# EJEMPLO DE 
y<-c(28,25,27,36,32,32,18,19,23,31,30,29)
A<-rep(c(rep(-1,3),rep(1,3)),2)
B<-c(rep(-1,6),rep(1,6))
AB<-A*B
ypA<-tapply(y,A,mean)
ypA
A<-ypA[2]-ypA[1]
A

ypB<-tapply(y,B,mean)
B

B<-ypB[2]-ypB[1]
B

ypAB<-tapply(y,AB,mean)

AB<-ypAB[2]-ypAB[1]
AB

# ANOVA EN EL DISEÑO 2K
y<-c(28,25,27,36,32,32,18,19,23,31,30,29)
A<-rep(c(rep(-1,3),rep(1,3)),2)
B<-c(rep(-1,6),rep(1,6))
mod<-lm(y~A+B+A*B)

# Otra forma de encontrar los efectos estimados

betaest<-coefficients(mod)
efectosest<-2*betaest[2:4]
data.frame(efectosest)

anva<-aov(mod)
summary(anva)

# PARA VERIFICAR SUPUESTOS 
mod1<-lm(y~A+B)
CME<-deviance(mod1)/df.residual(mod1)
beta1<-coefficients(mod1)
I<-rep(1,length(y))
X<-cbind(I,A,B)
yest<-X%*%beta1
e<-y-yest
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

# VARIANZA CONSTANTE BREUSCH PAGAN
library(car)
ncvTest(mod1)

# GRAFICO DE LA SUPERFICIE DE RESPUESTA
mod<-function(x1,x2){16.3333+0.83333*x1-5*x2}
x1<-seq(15,25,0.5)
x2<-seq(1,2,0.05)
z<-outer(x1,x2,mod)
par(mfrow=c(1,1))
persp(x1,x2,z,theta=-40,phi=30,ticktype="detailed",
      xlab="concentración del reactivo",
      ylab="cantidad del catalizador", 
      zlab="y")


