#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::        MUESTREADOR DE GIBBS        :::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set.seed(1)
x.1 <- rpois(40, 1) ## lambda = 1
m <- 40
x.2 <- rpois(30, 2.5) ## phi = 2.5
x <- c(x.1, x.2)

length(x)

par(mfrow = c(1,1))
plot(x, type = "l", xlab = "n", main = "Observaciones Poisson con punto de cambio")
points(seq(1:70), x, pch = 16, col = "blue")
grid(6, 6, lwd = 2)
n <- 70

# Hiperparametros
alpha.lambda <- 0.1
beta.lambda <- 0.1
alpha.phi <- 0.1
beta.phi <- 0.1

# Densidades condicionales
nsim <- 21000

# Matriz que guardara las simulaciones
A <- matrix(0, nsim, 3)

# Inicializar
A[1,] <- c(1,1,3)

# Inicia proceso gibbs

# Primer caso tm <- 2
# Suma del valor i hasta el numero del tamaño
#sum(x[A[2-1,3]:n])

for (i in 2:nsim) {
  tm <- sum(x[1:A[i-1, 3]])
  um <- sum(x[A[i-1,3]:n])
  A[i, 1] <- rgamma(1, alpha.lambda + tm, beta.lambda + A[i-1,3])
  l <- A[i, 1]
  A[i, 2] <- rgamma(1, alpha.phi + um, beta.phi + n - A[i-1,3]) #Recuerda A[i-1,3] = m 
  phi <- A[i, 2]
  # Construimos probabilidades 1:n
  f.aux <- rep(0, n)
  for (j in 1:n) {
    t.j <- sum(x[1:j])
    if (j < n) {
      u.j <- sum(x[(j+1):n])
    } else {
      u.j <- 0
    }
  f.aux[j] <- 1 ^ (alpha.lambda + t.j -1) * phi ^ (alpha.phi + u.j - 1) * exp(-(beta.lambda + j)*l)* exp(-(beta.phi + n-j)* phi)
  }
  f <- f.aux/sum(f.aux)
  A[i, 3] <- sample(1:n, 1, prob=f)
  if (i %% 1000 == 0) {
    print(i)
  }
}

burn <- 1001
A <- A[burn:nsim, ]
# Analisis de convergencia
par(mfrow = c(2, 2))
plot(seq(1, length(A[,1])), cumsum(A[, 1])/seq(1, length(A[,1])),
     type = "l", ylab = expression(lambda), xlab = 'n', cex.axis = 0.7,
     main = expression(paste("Promedio Ergodico ",lambda)))
grid(6, 6, lwd = 2)

plot(seq(1, length(A[,2])), cumsum(A[, 2])/seq(1, length(A[,2])),
     type = "l", ylab = expression(phi), xlab = 'n', cex.axis = 0.7,
     main = expression(paste("Promedio Ergodico ",phi)))
grid(6, 6, lwd = 2)

plot(seq(1, length(A[,3])), cumsum(A[, 3])/seq(1, length(A[,3])),
     type = "l", ylab = expression(m), xlab = 'n', cex.axis = 0.7,
     main = expression(paste("Promedio Ergodico ",m)))
grid(6, 6, lwd = 2)

B <- A

par(mfrow = c(2, 2))
acf(A[, 1], main = expression(paste("ACF: ",lambda)))
acf(A[, 2], main = expression(paste("ACF: ",phi)))
acf(A[, 3], main = expression(paste("ACF: ",m)))

mean(A[, 1])
mean(A[, 2])
mean(A[, 3])

par(mfrow = c(2, 2))
hist(A[,1],breaks=20,main=expression(paste("Histograma ", lambda )),
     xlab=expression(lambda),freq=FALSE,xlim=c(0,3))
hist(A[,2],breaks=20,xlim=c(0,5),
     main=expression(paste("Histograma ", phi )),
     xlab=expression(beta),freq=FALSE)
barplot(table(A[,3])/length(A[,3]),cex.axis=0.5,
        main=expression(paste("Histograma ", m )),xlab=expression(m))



## Segunda parte no sale
## Muestreador Gibbs
set.seed(1)
x.1 <- rpois(40,1) ## lambda=1
m=40
x.2 <- rpois(30,2.5) ## phi=2.5
x <- c(x.1,x.2)
length(x)
par(mfrow=c(1, 1))
plot(x,type="l",xlab="n",main="Observaciones Poisson con punto de cambio")
points(seq(1:70),x,pch=16,col="blue")
grid(6, 6, lwd = 2)
n <- 70
#Hiperparametros
alpha.l <- 0.1
beta.l <- 0.1
alpha.p <- 0.1
beta.p <- 0.1

#Densidades condicionales
nsim <- 21000
#Matriz que guardara las simulaciones
A <- matrix(0,nsim,3)
#inicializar
A[1,] <- c(1,1,3)
#Inicia proceso gibbs
for(i in 2:nsim){
  tm <- sum(x[1:A[i-1,3]])
  um <- sum(x[A[i-1,3]:n])
  A[i,1] <- rgamma(1,alpha.l+tm, beta.l+A[i-1,3])
  l<-A[i,1]
  A[i,2] <- rgamma(1,alpha.p+um, beta.p+n-A[i-1,3])
  phi <- A[i,2]
  #Construimos probabilidades 1:n
  f.aux <- rep(0,n)
  for(j in 1:n){
    t.j <- sum(x[1:j])
    if(j < n ){
      u.j <- sum(x[(j+1):n])
    } else{
      u.j <- 0
    }
    f.aux[j] <- l ^ (alpha.lambda + t.j -l) * phi ^ (alpha.phi + u.j - l) * exp(-(beta.lambda + j)*l)* exp(-(beta.phi + n-j)* phi)
  }
  f <- f.aux/sum(f.aux)
  A[i,3] <- sample(1:n,1,prob=f)
  if (i %% 1000==0){
    print(i)
  }
}
burn <- 1001
A <- A[burn:nsim,]
#Analisis de convergencia
par(mfrow=c(2, 2))
plot(seq(1,length(A[,1])),cumsum(A[,1])/seq(1,length(A[,1])),
     type='l', ylab=expression(lambda),xlab='n ',cex.axis=0.7,
     main=expression(paste("Promedio Ergodico ",lambda)))
grid(6, 6, lwd = 2)
plot(seq(1,length(A[,2])),cumsum(A[,2])/seq(1,length(A[,2])),
     type='l',ylab=expression(phi),xlab='n ',cex.axis=0.7,
     main=expression(paste("Promedio Ergodico ",phi)))
grid(6, 6, lwd = 2)
plot(seq(1,length(A[,3])),cumsum(A[,3])/seq(1,length(A[,3])),
     type='l',ylab=expression(m),xlab='n ',cex.axis=0.7,
     main=expression(paste("Promedio Ergodico ", m)))
grid(6, 6, lwd = 2)

B<-A
par(mfrow=c(2, 2))
acf(A[,1],main=expression(paste("ACF: ",lambda)))
acf(A[,2],main=expression(paste("ACF: ",phi)))
acf(A[,3],main=expression(paste("ACF: ",m)))
mean(A[,1])
mean(A[,2])
mean(A[,3])
par(mfrow=c(2, 2))
hist(A[,1],breaks=20,main=expression(paste("Histograma ", lambda )),
     xlab=expression(lambda),freq=FALSE,xlim=c(0,3))
hist(A[,2],breaks=20,xlim=c(0,5),
     main=expression(paste("Histograma ", phi )),
     xlab=expression(beta),freq=FALSE)
#par(mfrow=c(1, 1))
barplot(table(A[,3])/length(A[,3]),cex.axis=0.5,
        main=expression(paste("Histograma ", m )),xlab=expression(m))






