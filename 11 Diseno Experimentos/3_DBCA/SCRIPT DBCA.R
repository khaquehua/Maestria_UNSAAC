gasolina<-read.table("gasolina.txt",header=T)
head(gasolina)
rendimiento<-gasolina[,1]
vehiculo <- factor(gasolina[,2])
tipos <- gasolina[,3]

modeg<-lm(rendimiento~vehiculo+tipos)
anva<-anova(modeg)
anva
# calculando la eficiencia relativa
cm<-anva$Mean
sc<-anva$Sum
nt<-tapply(rendimiento,vehiculo,length)
t<-nt[1]
nb<-tapply(rendimiento,tipos,length)
b<-nb[1]
ER<-((sc[1]+b*(t-1)*cm[3])/(t*b-1))/cm[3]
ER 

# Comparaciones multiples

# prueba t (DMS = LSD = DLS)
# Ho: MU_D = Mu_E
modeg<-lm(rendimiento~vehiculo+tipos) 
efect<-modeg$coefficients
dmedia<-efect-efect[9]
dmedia<-dmedia[8]
dmedia

tc<-dmedia/sqrt(cm[3]*(2/5))
tc

pvalue<-2*pt(tc,df.residual(modeg))
pvalue

# comparacion multiple de Tukey
gasolina<-read.table("gasolina.txt",header=T)
library(multcomp)
nd<-tapply(rendimiento,tipos,length)
mt<-contrMat(nd,type="Tukey") # matriz tukey comparaciones

library(agricolae)
gasolina$bloques = factor(gasolina$bloques)
gasolina$aditivo = factor(gasolina$aditivo)
modg = aov(rendimiento~aditivo+bloques, data = gasolina)
summary(modg)
comp =  HSD.test(modg,"aditivo", group = T, 
                            console = T)
comp
# Explicacion obtencion el qtukey
# Valor crítico de Tukey (distribución studentized range)
# Ejemplo: alfa = 0.05, número de grupos = 4, grados de libertad = 20
qtukey(0.95, nmeans = 5, df = 16) # nmeans=número de grupos = 4

library(multcomp)
summary(glht(modg, linfct = mcp(aditivo = "Dunnett")))






