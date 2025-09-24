#::::::::::::    SCRIPT KEVIN HAQUEHUA     :::::::::::::::::::::::::::::::::::::
# LIBRERIAS A UTILIZAR
library(readxl)
library(here)
library(agricolae)
library(multcomp)
library(emmeans)
library(gmodels)

# LECTURA DE DATOS
data <- read_excel(here("11 Diseno Experimentos/Examen_2/data_papa.xlsx"))
head(data)

# REALIZACION DEL MODELO Y ANOVA
attach(data)
modeg<-lm(rendimiento~suelo+formula)
anva<-anova(modeg)
anva

# VER LOS COEFICIENTE A PARTIR DE F1 Y S1
modeg
cm<-anva$Mean #Valores de la tabla anova

# COMPARAR
efect<-modeg$coefficients
dmedia<-efect-efect[6]
dmedia<-dmedia[4]
dmedia

tc<-dmedia/sqrt(cm[3]*(2/5))
tc

pvalue<-2*pt(tc,df.residual(modeg))
pvalue

# COMPARACIÓN DE TUKEY
model_formula <- aov(rendimiento ~ formula, data = data)
TukeyHSD(model_formula, "formula")

# PRUBE DE DUNNET
data$formula <- factor(data$formula)
data$suelo <- factor(data$suelo)
modg = aov(rendimiento~formula+suelo, data = data)
summary(glht(modg, linfct = mcp(formula = "Dunnett")))

# REALIZAR TODAS LAS COMPARACIONES
medias <- emmeans(modg, ~ formula)
comparaciones_tukey <- contrast(medias, method = "pairwise", adjust = "tukey")
summary(comparaciones_tukey)  

# REALIZAR EL CONTRASTE
data$formula <- factor(data$formula, levels = c("f1","f2","f3","f4"))
con = c(1, 0, -0.5, -0.5)
fit.contrast(modg , "formula", con)

