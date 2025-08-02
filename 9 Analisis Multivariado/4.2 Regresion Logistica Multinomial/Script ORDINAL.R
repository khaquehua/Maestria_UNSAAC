# Regresión Logistica Ordinal

dat = read.table("https://raw.githubusercontent.com/alanagresti/categorical-data/master/Mental2.dat")
names( dat ) = c("estado" , "estatus" , "sucesos")
dat$estado = as.ordered (dat$estado)
levels( dat$estado ) = c("well" , "mild" , "mod" , "imp")
dat$estatus = as.factor (dat$estatus)
levels( dat$estatus ) = c("low" , "high")

# verificar el orden
str(dat$estado)
#dat$estado <- factor(dat$estado, levels = rev(levels(dat$estado)))
# str(dat$estado)

# modelar con la regresion logistica ordinal
# modelos acumulados
# odds proporcionales

library(MASS)
?polr
fit = polr(estado ~ estatus + sucesos ,data= dat)
summary(fit)

tabla_c <- coef(summary(fit))
p <- pnorm(abs(tabla_c[, "t value"]), lower.tail = FALSE) * 2
tabla_c <- cbind(tabla_c, "p value" = p)
tabla_c


# Se calculan ahora intervalos de conﬁanza para los coeﬁcientes originales
# y su expresión en términos de la exponencial.

#Intervalos de confianza
(ci=confint(fit))

#Exponencial de los parametros e IC
exp(cbind(OR=coef(fit),ci))

#Seleccionas los de estatus 1 y mas de 5 sucesos
que =subset(dat, estatus=="high"& sucesos>5)
#Prediccion para el grupo anterior
cbind(que,predict(fit ,data.frame(que), type="probs"))

nuevo= data.frame(estatus="low", sucesos=1)
nuevo2= data.frame(estatus="high", sucesos=0)
predict(fit ,nuevo, type="probs")
predict(fit ,nuevo2, type="probs")

nuevo= data.frame(estatus="low", sucesos=1)
nuevo2= data.frame(estatus="high", sucesos=1)
predict(fit ,nuevo, type="probs")
predict(fit ,nuevo2, type="probs")

(exp(-0.2819-0.3189*4.275))/(1 + exp( -0.3189*4.275 - 0.2819))

(exp(-0.2819-0.3189*4.275 +1.1112))/(1 + exp(- 0.2819 -0.3189*4.275 +1.1112))

(exp(-0.2819-0.3189*9 +1.1112*0))/(1 + exp(- 0.2819 -0.3189*9 +1.1112*0))
(exp(-0.2819-0.3189*9 +1.1112*1))/(1 + exp(- 0.2819 -0.3189*9 +1.1112*1))



## Otra opción es usar la función vglm. Esta función está en la librería VGAM.
library(VGAM)
fit2 = vglm(estado~estatus + sucesos,
             family=cumulative(parallel=TRUE),data=dat)
summary(fit2)
mean(dat$sucesos)


