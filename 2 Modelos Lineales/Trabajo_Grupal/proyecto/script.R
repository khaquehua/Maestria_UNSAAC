library(faraway)
data(coagulation, package="faraway")
coagulation

plot(coag ~ diet, coagulation,ylab="coagulation time")

lmod <- lm(coag ~ diet, coagulation)
anova(lmod)

(tci <- TukeyHSD(aov(coag ~ diet, coagulation)))
