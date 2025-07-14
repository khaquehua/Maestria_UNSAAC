A<-matrix(c(1,0,-1,0,2,4,-1,2,5,1,2,3),ncol=4) ; A
E1<-matrix(c(1,0,1,0,1,0,0,0,1),ncol=3) # F3 + F1
E1%*%A
E2<-matrix(c(1,0,0,0,1,-2,0,0,1),ncol=3) # F3-2F2
E2%*%E1%*%A
P<-E2%*%E1

P%*%A

PA<-P%*%A
G<-rbind(cbind(solve(PA[1:2,1:2]),rep(0,2)),t(rep(0,3)),t(rep(0,3)))%*%P
G

A%*%G%*%A
A


E3<-matrix(c(1,0,0,0,0,1,0,0,1,0,1,0,-1,0,0,1),ncol=4)
P%*%A%*%E3
E4<-matrix(c(1,0,0,0, 0,1,0,0,  0,-1,1,0,0,-1,0,1),ncol=4)
P%*%A%*%E3%*%E4

# 
Q<-E3%*%E4
Q
P%*%A%*%Q


A<-matrix(c(1,0,-1,0,2,4,-1,2,5,1,2,3),ncol=4)
A
qr(A)$rank
library(MASS)
G=ginv(A)

A%*%G%*%A
A

datos= read.delim("clipboard", T)

# vector de medias
colMeans(datos)
# matriz de varianzas y covarianzas
cov(datos)
# matriz de correlaciones
cor(datos)

# practica de la distribucion normal
# pregunta 1
mu = matrix(c(1,-1),2,1)
mu
S= matrix(c(12.5,5.5,5.5,20.3),2,2)
S
A= matrix(c(2,1/2,1,-1/2),2,2)
A

mu2=A%*%mu
mu2
S2 = A%*%S%*%t(A)
S2

pnorm(19.83,mean = 1,sd = sqrt(92.3), lower.tail = F)

# pregunta 2
# parte a
mu = matrix(c(6,3,2,-7),4,1)
mu
S= matrix(c(4,-3,0,-1,-3,6,5,5,
            0,5,8,8,-1,5,8,11),4,4)
S
A= matrix(c(4,-2,6,3),1,4)
A

mu2=A%*%mu
mu2
S2 = A%*%S%*%t(A)
S2

# parte b
A1= matrix(c(2,5,1,-1,2,-2,-8,7),2,4)
A1

muz=A1%*%mu
muz
Sz = A1%*%S%*%t(A)
Sz
#parte c
A2= matrix(c(2,1,1,-1,2,1,1,-3,2,2,5,3),3,4)
A2

mu2=A2%*%mu
mu2
S2 = A2%*%S%*%t(A2)
S2
#covzw=t(z)s*w
A1%*%S%*%t(A2)
