A = matrix(c(2,1,1,-1), 2, 2) 
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

A <- matrix(c(1,3,5,2,-1,-4,4,2,0,3,-2,-7),ncol=4)
E3 <- matrix(c(3,0,0,0,1,0,0,0,1),ncol=3)
E3%*%A

E2 <- matrix(c(1,-1,0,0,1,0,0,0,1),ncol=3)
E2%*%E3%*%A

E1 <- matrix(c(1/3,0,0,0,1,0,0,0,1),ncol=3)
E1%*%E2%*%E3%*%A

P <- E1%*%E2%*%E3
P

# FORMA CUADRATICA EQUIVALENTE
A1 <- matrix(c(1,3,5,2,4,5,2,5,6,-1,2,4),3,4)
A1

#columna 2 - 2 col1
E1 <- matrix(c(2,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4)
E1
E2 <- matrix(c(1,0,0,0,-1,1,0,0,0,0,1,0,0,0,0,1),4,4)
E2
E3 <- matrix(c(1/2,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4)
E3

P <- E1 %*% E2 %*% E3
P

A1 %*% P

A1 %*% P %*% P

P2 <- matrix(c(1,0,0,0,0,1,0,0,-2,0,1,0,0,0,0,1),4,4)
P2

A1 %*% P %*% P2

P3 <- matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,1,0,0,1),4,4)
P3

A1 %*% P %*% P2 %*% P3

F1 <- matrix(c(1,-3,0,0,1,0,0,0,1),3,3)
F1

F1 %*% A1 %*% P %*% P2 %*% P3

F2 <- matrix(c(1,0,-5,0,1,0,0,0,1),3,3)
F2

F2 %*% F1 %*% A1 %*% P %*% P2 %*% P3


F3 <- matrix(c(1,0,0,0,-1/2,0,0,0,1),3,3)
F3

F3 %*% F2 %*% F1 %*% A1 %*% P %*% P2 %*% P3

F4 <- matrix(c(1,0,0,0,1,5,0,0,1),3,3)
F4
F4 %*% F3 %*% F2 %*% F1 %*% A1 %*% P %*% P2 %*% P3

F5 <- matrix(c(1,0,-2/3,0,1,0,0,0,1),3,3)
F5
F5 %*% F4 %*% F3 %*% F2 %*% F1 %*% A1 %*% P %*% P2 %*% P3


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Solución del ejercicio 3
# A
(mu <- matrix(c(27,10,18),3,1))
(S <- matrix(c(9,2,1,2,1,-1,1,-1,4),3,3))
(A <- matrix(c(145,303,175),1,3))

muP <- A %*% mu # Valor esperado del portafolio
SP <- A %*% S %*% t(A) # Varianza del portafolio

muP
SP

# B
pnorm(11000, mean = 10095, sd = sqrt(523974), lower.tail = F)












