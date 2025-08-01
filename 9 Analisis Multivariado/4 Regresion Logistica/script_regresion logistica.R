library(Fahrmeir)
data(credit)

write.csv(credit,"creditos.csv")

# Estimacion
modelo_logistic <- glm(Y ~ ., family=binomial,data=credit)
summary(modelo_logistic)

################################################################################
#Interpretación:
# - Para cada mes adicional en la duración del crédito, el logaritmo de la ventaja
#   de ser mal pagador (versus ser bueno) se incrementa en 0.03503
#
# - Las variables categóricas tiene una interpretación distinta:
#   Por ejemplo, si el cliente es hombre, el logaritmo de las ventaja de ser 
#   mal pagador disminuye en -0.2235 en comparación a si fuera mujer
################################################################################

# Intervalos de Confianza para los coeficientes
confint(modelo_logistic)

options(scipen=99) # para deshabilitar notacion cientifica
## solo tasa de ventajas
exp(coef(modelo_logistic))
## tasa de ventajas e IC 95% 
exp(cbind(OR = coef(modelo_logistic), confint(modelo_logistic)))

################################################################################
#Interpretación:
# - De los resultados es posible afirmar que por cada mes adicional en la duraci?n 
#   del crédito la ventaja de ser un mal cliente (frente a ser buen pagador)
#   se incrementa en un 3.5%.
# - Si el cliente es hombre, la ventaja de ser mal pagador disminuye en un 20%
#   en comparación a si fuera mujer.
################################################################################

# Selección de Variables
library(MASS)
modelo_logistic <- stepAIC(modelo_logistic)
summary(modelo_logistic)

# Calidad de Ajuste (Prueba de Hosmer y Lemeshow)
library(ResourceSelection)
hl <- hoslem.test(modelo_logistic$y, fitted(modelo_logistic), g=10)
hl
# H_o: no hay diferencia entre observado y lo esperado
# H_a: Si hay diferencia(no ajusta el modelo al cojunto de datos)
# acepto la H_o por p.valor>0.05 el modelo si se ajusta bien a 
# los datos



#################################
# Evaluación de la Predicción   #
#################################

## Matriz de confusión (error Clasificación)

# Valores predichos de Y
yprob<-predict(modelo_logistic,type="response")
head(yprob)

# Otra forma:
ypred <- as.numeric(yprob >= 0.5 )
ypred <- factor(ypred, labels = levels(credit$Y))

# Matriz de Confusión
(mc<-table(ypred,credit$Y))
(637+120)/1000 # tasa de exactitud, tasa de buena clasification , accuracy
# tasa de error
1-(637+120)/1000
# otra forma de calcular
testerr <- mean(ypred!=credit$Y)
testerr

library(caret)
# cofusionMatrix(data= vari predicha factor, reference = es la clase)  el estimador accuracy es bueno??
confusionMatrix(data = ypred,reference = credit$Y, positive = "mal")
confusionMatrix(data = ypred,reference = credit$Y, positive = "mal", mode = "prec_recall")
confusionMatrix(data = ypred,reference = credit$Y, positive = "mal", mode = "everything")
help(confusionMatrix)


## Curvas ROC
library(pROC)
# Area debajo de la curva ROC
analysis <- roc(response=credit$Y, predictor=yprob)
analysis
MLmetrics::AUC(y_pred = yprob,y_true =as.numeric(credit$Y)-1 )

# Coeficiente Gini
2*analysis$auc-1
MLmetrics::Gini(y_pred = yprob,y_true =as.numeric(credit$Y)-1 )


# Grafica de la Curva ROC
plot(1-analysis$specificities,analysis$sensitivities,type="l",
     ylab="Sensitividad",xlab="1-Especificidad",col="blue",lwd=2,
     main = "Curva ROC para el modelo logistico")
abline(a=0,b=1, col = "red")


##### importante #####
##########################
####

# Hallar punto de corte #### para calibrar el modelo sacrificas el accuracy pero balanceas la especificidad y sensibilidad
# Usando el criterio del indice J de Youden
# J = Sensitivity + Specificity - 1
e <- cbind(analysis$thresholds,analysis$sensitivities+analysis$specificities-1)
head(e)
opt_t <- subset(e,e[,2]==max(e[,2]))[,1] # buscamos el maximo valor de Jouden
opt_t

ypred2 <- factor(as.numeric(yprob >= opt_t ), labels = levels(credit$Y))
confusionMatrix(ypred2,credit$Y, positive = "mal")

# Otra forma
coords(analysis , "b", ret="t", best.method="youden", transpose = TRUE) 

library(InformationValue)
InformationValue::plotROC(actuals = as.numeric(credit$Y)-1, predictedScores = yprob)
InformationValue::AUROC(actuals = as.numeric(credit$Y)-1, predictedScores = yprob)
InformationValue::optimalCutoff(actuals = as.numeric(credit$Y)-1, predictedScores = yprob, optimiseFor = "Both")
InformationValue::optimalCutoff(actuals = as.numeric(credit$Y)-1, predictedScores = yprob, optimiseFor = "Ones")
InformationValue::optimalCutoff(actuals = as.numeric(credit$Y)-1, predictedScores = yprob, optimiseFor = "Zeros")

#Base de datos con las probabilidades y categorias predichas
credit$prob_log<- yprob
credit$ypred_log<- ypred
head(credit)
