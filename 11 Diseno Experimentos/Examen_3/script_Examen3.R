#::::::::::::    SCRIPT KEVIN HAQUEHUA EXAMEN 3    ::::::::::::::::::::::::::::
# LIBRERIAS A UTILIZAR
library(nortest)
library(car)

# GENERACION DE LOS DATOS Y EFECTOS
y<-c(22, 32, 35, 55, 44, 40, 60, 39, 31, 43, 34, 47, 45, 37, 50, 41, 25, 29, 50, 46, 38, 36, 54, 47)
A<-rep(c(-1,1),12)
B<-rep(c(rep(-1,2),rep(1,2)),6)
C<-rep(c(rep(-1,4),rep(1,4)),3)

# INTERACCIONES
AB <- A*B
BC <- B*C
AC <- A*C
ABC <- A*B*C 

# PARA VER LOS DIFERENTES EFECTOS
# A
ypA<-tapply(y,A,mean)
ypA
Aefe<-ypA[2]-ypA[1]
Aefe
# B
ypB<-tapply(y,B,mean)
ypB
Befe<-ypB[2]-ypB[1]
Befe
# C
ypC<-tapply(y,C,mean)
ypC
Cefe<-ypC[2]-ypC[1]
Cefe
# AB
ypAB<-tapply(y,AB,mean)
ypAB
ABefe<-ypAB[2]-ypAB[1]
ABefe
# AC
ypAC<-tapply(y,AC,mean)
ypAC
ACefe<-ypAC[2]-ypAC[1]
ACefe
# BC
ypBC<-tapply(y,BC,mean)
ypBC
BCefe<-ypBC[2]-ypBC[1]
BCefe
# ABC
ypABC<-tapply(y,ABC,mean)
ypABC
ABCefe<-ypABC[2]-ypABC[1]
ABCefe

# OBTENEMOS EL MODELO
mod<-lm(y~A+B+C+ A*B+A*C+B*C + A*B*C)
# sumas de cuadrados y ANVA
anva<-aov(mod)
summary(anva)

# MODELO DE REGRESION GENERADO
mod1<-lm(y~B+C+A*C)
summary(mod1)

# HALLAR LOS CME Y RESIDUOS DEL MODELO
CME<-deviance(mod1)/df.residual(mod1)
beta1<-coefficients(mod1)
I<-rep(1,length(y))
X<-cbind(I,B,C,A,AC)
yest<-X%*%beta1
e<- y-yest
e

H<-X%*%solve(t(X)%*%X)%*%t(X)
ri<-e/sqrt(CME*(1-diag(H)))
ri

H<-X%*%solve(t(X)%*%X)%*%t(X)
ri<-e/sqrt(CME*(1-diag(H)))
ri

# COMPROBACION DE SUPUESTOS
# grafico
par(mfrow=c(2,2))
plot(mod1)
# NORMALIDAD
ri<-rstandard(mod1)
shapiro.test(ri) #SHAPIRO
ad.test(ri) #ANDERSON
# VARIANZA CONSTANTE
ncvTest(mod1) # BREUCH PAGAN

# GRAFICO DE SUPERFICIE DE RESPUESTA
modBC<-function(x2,x3){40.83+5.67*x2+3.42*x3}
x2<-seq(-5,5,0.05)
x3<-seq(-5,5,0.05)
z<-outer(x2,x3,modBC)
par(mfrow=c(1,1))
persp(x2,x3,z,theta=-40,phi=30,ticktype="detailed",
      xlab="factorB",
      ylab="factorC", 
      zlab="y")

