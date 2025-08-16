#-------------------------------------------------------------
#                 REGRESIÓN LOGISTICA
#-----------------------------------------------------------
#llamamos la base de datos
library(readxl)
Datos <- read_excel("10 Mineria de Datos/Regresion_Logistica/Datos.xlsx")
View(Datos)

# Se llama a la libreria ggplot2
library(ggplot2)

# Conocer cuántos diagnósticos hay de tumores benignos(0) y malignos (1)
table(Datos$Diag)

#Ajuste del modelo
m1 <- glm(Diag ~ Radio + Textura + Perimetro + Area + GSuavizacion +
            Compactacion + Concavidad + PConcavidad + Simetria ,
          data = Datos,
          family = binomial)
summary(m1)

round(cor(Datos), digits = 2)

# Calcular medidas de bondad de ajuste
#install.packages("pscl")
library(pscl)
pR2(m1)
AIC(m1)
BIC(m1)

# Modelo m1 estandarizado
library(car)
m1z <- glm(Diag ~ scale(Radio) + scale(Textura) + scale(Perimetro) + scale(Area) +
             scale(GSuavizacion) + scale(Compactacion) + scale(Concavidad) +
             scale(PConcavidad) + scale(Simetria),
           data=Datos, family=binomial)
car::vif(m1z)
pR2(m1z)
AIC(m1z)
BIC(m1z)

#Se mejora el modelo excluyendo Xi cuyo p-values no es significativo
m2 <- glm(Diag ~ Textura + Area + GSuavizacion +
            PConcavidad + Simetria,
          data = Datos,
          family = binomial)
summary(m2)

# Calcular medidas de bondad de ajuste
pR2(m2)
AIC(m2)
BIC(m2)
# Interpretacion:

# B(Textura) = -0.05386: si la textura del tumor benigno de un paciente disminuye en un año, por tanto tambien disminuye
#                    la probabilidad de tener cáncer de mamá  vs tener cáncer maligno:  exp(-0.2436) = 0.584

#Calculo de los odds

round(exp(coefficients(m2)), digits = 3)
#Calculo de odds en tabla
library(cli)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(m2, auto.label = FALSE)

#Intervalos de confianza al 95%
confint(m2) 

#Intevalo de confianza de las razones de probabilidad al 95% para el Modelo2
round(exp(confint(m2, level=0.95)),digits = 3)

#Para evaluar el modelo,comparar el valor real con el predicho por el modelo
anova(m2, test ='Chisq')
#----------------------------------------------------------
#                     Evaluación de residuos 
#----------------------------------------------------------
# Diferencia de residuos
dif_residuos <- m2$null.deviance - m2$deviance

# Grados libertad
df <-m2$df.null - m2$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))

#----------------------------------------------------------
#Comparación de las predicciones con las observaciones
#----------------------------------------------------------
library(vcd)
predicciones <- ifelse(test = m2$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(m2$model$Diag, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))
clasf<-((193+345)/(193+13+10+345))*100
clasf
#Sin utilizar los números
tn <- matriz_confusion["0","0"]; tp <- matriz_confusion["1","1"]
fn <- matriz_confusion["1","0"]; fp <- matriz_confusion["0","1"]

accuracy <- (tp + tn) / sum(matriz_confusion)
sens     <- tp / (tp + fn)   # recall clase 1
espec    <- tn / (tn + fp)
ppv      <- tp / (tp + fp)   # precisión
npv      <- tn / (tn + fn)

c(accuracy=accuracy, sensibilidad=sens, especificidad=espec, PPV=ppv, NPV=npv)


#-----------------------------------------------------------------
#                             CURVA ROC
#-----------------------------------------------------------
#curva ROC
# Asegúrate de que 1 sea la clase "evento" (maligno)
Datos$Diag <- factor(Datos$Diag, levels = c(0,1))

# Usa probabilidades del modelo
prob <- m2$fitted.values

roc_m2 <- roc(response = Datos$Diag, predictor = prob, ci=TRUE, auc=TRUE, quiet=TRUE)
roc_m2
ci.auc(roc_m2)
plot.roc(roc_m2, print.auc=TRUE, print.thres="best", col = "blue",
         xlab="1 - Especificidad", ylab="Sensibilidad")



