library(MASS)
library(nnet)
comen.f =factor(c("peces","invert","reptiles","pajaritos" ,"otros"),
                levels=c("peces","invert","reptiles","pajaritos","otros"))
tamano.f =factor(c("<2.3", ">2.3"),levels=c(">2.3" , "<2.3" ))
sexo.f =factor(c("m","f"),levels=c("m" ,"f"))

charca.f =factor(c("hancock" , "oklawaha" ,"trafford" , "george"),
                 levels=c("george" ,"hancock" ,"oklawaha" ,"trafford" ))

tabla.array = expand.grid ( comen = comen.f , tamano = tamano.f , sexo = sexo.f ,
                            charca = charca.f )

temp = c (7,1, 0, 0, 5, 4, 0, 0, 1, 2, 16 , 3, 2, 2, 3, 3, 0, 1, 2, 3,
          2, 2, 0, 0, 1, 13 , 7, 6, 0, 0, 3, 9, 1, 0, 2, 0, 1, 0, 1, 0, 3, 7, 1,
          0, 1, 8, 6, 6, 3, 5, 2, 4, 1, 1, 4, 0, 1, 0, 0, 0, 13 , 10 , 0, 2, 2,
          9, 0, 0, 1, 2, 3, 9, 1, 0, 1, 8, 1, 0, 0, 1)
tabla =tapply(temp, tabla.array[, 1:4],sum)
library(vcd)
structable(~charca + sexo + tamano + comen ,tabla)
mosaic (~charca + sexo + tamano + comen , tabla , shade = TRUE )

oc = options(contrasts = c("contr.treatment", "contr.poly"))
fits1 = nnet::multinom(comen~tamano,data=tabla.array,weights=temp)
summary( fits1 ,cor=F)


# comparaciones mediante la devianza
library(nnet)
fitS = multinom ( comen ~ charca*tamano*sexo ,data= tabla.array ,
                  weights= temp )
fitS2 = multinom ( comen ~ charca*tamano ,data= tabla.array ,
                   weights= temp )
anova(fitS2,fitS,test ="Chisq")

# modelo nulo
fit0 = multinom ( comen ~ 1,data= tabla.array ,weights= temp )
fit1 = multinom ( comen ~ charca ,data= tabla.array ,weights= temp )
fit2 = multinom ( comen ~ tamano ,data= tabla.array ,weights= temp )
fit3 = multinom ( comen ~ sexo ,data= tabla.array ,weights= temp )
fit4 = multinom ( comen ~ charca + tamano ,data= tabla.array ,
                  weights= temp )
fit5 = multinom ( comen ~ charca + tamano + sexo ,data= tabla.array ,
                  weights= temp )

# Ho: el nulo es el mejor modelo
# Ha: el modelo considerando charca es mejor modelo
anova(fit0,fit1)
# conclusion:el modelo considerando charca es mejor modelo

anova(fit1,fit4) # fit4 es mejor modelo que fit1
anova(fit1,fit5) # fit5 es mejor modelo que fit1

anova(fit4,fit5) # fit4 es mejor modelo que fit5

# como resultado final del analisis regresion multinomial
summary(fit4)

NUEVO = data.frame(tamano=">2.3", sexo="m",charca="oklawaha")
predict(fit4, NUEVO, type= "probs")
?predict
