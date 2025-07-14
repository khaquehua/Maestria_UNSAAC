# como utilizar el R en matrices

A =matrix(c(2,1,1,-1), 2, 2) 
A
# determinante
det(A)
# inversa de la matriz
G=solve(A)
G
# solucion 
U= matrix(c(5,1),2,1)
U
G%*%U



A<-matrix(c(1,3,5,2,-1,-4,4,2,0,3,-2,-7),ncol=4)
A
E3<-matrix(c(3,0,0,0,1,0,0,0,1),ncol=3)
E3%*%A
E2<-matrix(c(1,-1,0,0,1,0,0,0,1),ncol=3)
E2%*%E3%*%A
E1<-matrix(c(1/3,0,0,0,1,0,0,0,1),ncol=3)
E1
E1%*%E2%*%E3%*%A
P<-E1%*%E2%*%E3
P

F3=matrix(c(5,0,0,0,1,0,0,0,1),ncol=3)
F2=matrix(c(1,0,-1,0,1,0,0,0,1),ncol=3)
F1=matrix(c(1/5,0,0,0,1,0,0,0,1),ncol=3)
F3%*%A
F2%*%F3%*%A
F1%*%F2%*%F3%*%A
F1%*%F2%*%F3%*%P%*%A

