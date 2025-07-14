library(readxl)
datos <- read_excel("D:/0_POS GRADO UNSAAC/MULTIVARIANTE-JULIO 2025/datospesotalla.xlsx", 
                    range = "B1:C21")
# datos del  ejercicio
mu0 = matrix(c(70,170),2,1) ; mu0
var = matrix(c(20,100,100,1000),2,2) ; var
n=length(datos$X1) ; n
p=dim(datos)[2]
p
# datos de la muestra
xbar=colMeans(datos)
xbar
# estadistico de prueba
chic= n*(t(xbar-mu0))%*%solve(var)%*%(xbar-mu0)
chic

# a metodologia region critica
qchisq(0.95,df = 2)
# b. metodologia pvalor
pchisq(8.4026, df = 2,lower.tail = F)


# ejemplo 2

mu0 = matrix(c(12,4,2),3,1); mu0

xbar = matrix(c(11.5,4.3,1.2),3,1); xbar

S=matrix(c(10,4,-5,4,12,-3,-5,-3,4),3,3)
S
n=20
p=3

T2= n*(t(xbar-mu0))%*%solve(S)%*%(xbar-mu0)
T2
Fc=((n-p)/(p*(n-1)))*T2
Fc

# prueba univariada para x2
tc= (4.3-4)/sqrt(12/20)
tc

# ejemplo 3
n1=32
n2=32
p=4
xbar1=matrix(c(15.97,15.91,27.19,22.75),4,1)
xbar2=matrix(c(12.34,13.91,16.59,21.94),4,1)

S1=matrix(c(5.192,4.545,6.522,5.250,
            4.545,13.18,6.76,6.266,
            6.522,6.760,28.67,14.47,
            5.25,6.266,14.47,16.65),4,4)
S2=matrix(c(9.136,7.549,5.531,4.151,
            7.549,18.60,5.446,5.446,
            5.531,5.446,13.55,13.55,
            4.151,5.446,13.55,28),4,4)

# calculo de la matriz de var y cov conjunta
Sp= ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)
Sp
# estadistico de prueba
T2=((n1*n2)/(n1+n2))* (t(xbar1-xbar2))%*%solve(Sp)%*%(xbar1-xbar2)
T2
qf(0.95,4,59)
n=n1+n2
Fc=((n-p-1)/(p*(n-2)))*T2
Fc
