#Analisis discriminante
#-------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(caret)
library(readxl)
library(psych)
data("iris")
View(iris)

#dividir la data
set.seed(12345)
muestra = createDataPartition(iris$Species, p =0.8, list =F)
train = iris[muestra,]
test = iris[-muestra,]

#analizar las distribuciones de cada especie 
p1<-ggplot(train, aes(Sepal.Length, fill =Species))+
       geom_density(alpha= 0.8)
p2<-ggplot(train, aes(Sepal.Width, fill =Species))+
       geom_density(alpha= 0.8)
p3<-ggplot(train, aes(Petal.Length, fill =Species))+
       geom_density(alpha= 0.8)
p4<-ggplot(train, aes(Petal.Width, fill =Species))+
       geom_density(alpha= 0.8)
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
  
                     
ggplot(train , aes(Petal.Width, Sepal.Length,color =Species))+
                          geom_point()
pairs(train[,-5], col = train$Species, psch =19)

#Normalidad multivariada
psych::mardia(iris[, 1:4])  # devuelve skew, kurtosis y p-values

#Normalidad univariada por especie (Shapiro-Wilk)
                       
by(iris[, 1:4], iris$Species, function(df) {
      sapply(as.data.frame(df), function(x) shapiro.test(x)$p.value)
})
                     
#test de Homogeneidad de la varianza
#test M de box
#Ho: la matriz Cov es igual a todos los grupos
library(biotools)
boxM(data =train[,-5], grouping = train[,5])
                     
#Atípicos multivariados (Mahalanobis) por especie
maha_counts <- by(iris[, 1:4], iris$Species, function(X) {
  d2 <- mahalanobis(X, colMeans(X), cov(X))
  sum(d2 > qchisq(0.999, df = 4))   # umbral estricto 99.9%
})
maha_counts

#-----Discriminate Lineal -----
library(MASS)
discrim_l = lda(Species ~ Sepal.Length + Sepal.Width +Petal.Length + Petal.Width,
               data =train)
discrim_l

#install.packages("klaR")
library(klaR)
partimat(Species ~ Sepal.Length + Sepal.Width +Petal.Length + Petal.Width,
         data =train, method = "lda", image.colors = c("skyblue", "lightgrey", "pink"), col.mean="red")

#evaluacion
prediccion = predict(discrim_l, test)
prediccion$class
prediccion$posterior
prediccion$x

#matriz de confusion
confusionMatrix(test$Species, prediccion$class)

#-----Discriminate cuadratico -----

discrim_q = qda(Species ~ Sepal.Length + Sepal.Width +Petal.Length + Petal.Width,
                data =train)


discrim_q

partimat(Species ~ Sepal.Length + Sepal.Width +Petal.Length + Petal.Width,
         data =train, method = "qda", image.colors = c("skyblue", "lightgrey", "pink"), col.mean="red")


#evaluacion
prediccion_q = predict(discrim_q, test)
prediccion_q$class
prediccion_q$posterior
prediccion_q$x

#matriz de confusion
confusionMatrix(test$Species, prediccion_q$class)

