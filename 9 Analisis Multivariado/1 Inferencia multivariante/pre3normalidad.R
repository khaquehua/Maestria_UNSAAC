# solucion del ejercicio 3
#a
(mu=matrix(c(27,10,18),3,1))
(S=matrix(c(9,2,1,2,1,-1,1,-1,4),3,3))
(A = matrix(c(145,303,175), 1,3))

muP= A%*%mu    # esperado del potafolio
SP= A%*%S%*%t(A) # varianza del portafolio

# b
pnorm(11000, mean = 10095, sd = sqrt(523974), lower.tail = F)















