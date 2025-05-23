## Métodos Monte  Carlo vía cardenas de Markok
## Generar una cadena de Markov

#Dada la siguiente función de densidad: f(x) = 0.3x, construir una cadena 
#que tenga una distribución estacionario o distribución de equilibrio f(x)

x <- NULL
x
x[1] = rnorm(1,0,1)
for (i in 2:2000) {
  x[i] = 0.3 * x[(i-1)] + rnorm(1,0,1)
}

#Gráfica de la cadena
plot(ts(x), col = "hotpink", xlab = "Estados t", ylab = "Valores de los estados", 
     main = "Cadena de Markov para f(x) = 0.3x")

## Forma empirica de verificar la convergencia
#1. Promedio Ergodico
mergod = cumsum(x)/(1:length(x))
plot(1:2000, mergod, col = "red", cex=0.7, type = "l",
     ylab = "Promedio de la cadena", xlab = "t")

#2. No correlación serial
acf(x)

#3. Histograma de los datos simulados
hist(x, breaks = 50, prob = T)
grid = seq(-8, 8, 0.01)
var.x = 1/(1-0.3^2)
lines(grid, dnorm(grid, 0, sqrt(var.x)), col = "hotpink", lwd = 3)




#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Formar grupo de 3 integrantes, cada grupo va a proponer simular valores de variables
#aleatorias por el metodo de la inversa, aceptacion-rechazo o composicion

#A ese caso se nos va a brindar la distribucion compleja
#Simular los valores de la distribucion que se va a brindar

#Encontrar por el metodo de la composicion, pero hacer el mismo proceso utilizando
#Por el metodo de la inversa

#Aplicar la inversa a todo o solo cada valor, pero como se realiza

#Buscar puntos de equilibrio


# Proponer 2 ejercicios: Utilizar uno de los metodos 