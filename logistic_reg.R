library(broom)
library(tidyverse)


d<-read_csv("data.csv")
head(d)


#creating new binary column based on limb phase
#limb phase >.5 = DS, in this situation 1=DS
d$LphsBin<-as.numeric(d$MeanLphsMS > .5)

#binary logistic regressions for all species as a whole, and each individual species
#need to add other substrate data
r<-glm(formula = LphsBin ~ scaled_Diam + V_1 + V_2 + V_3 + V_4,
       family = "binomial", data = d)
summary(r)

r1<-glm(formula = LphsBin ~ scaled_Diam,
       family = "binomial", data =subset(d, Species %in% "Ateles belzebuth"))
summary(r1)

r2<-glm(formula = LphsBin ~ scaled_Diam,
       family = "binomial", data =subset(d, Species %in% "Ateles geoffroyi"))
summary(r2)

r3<-glm(formula = LphsBin ~ scaled_Diam,
       family = "binomial", data =subset(d, Species %in% "Lagothrix lagotricha"))
summary(r3)

r4<-glm(formula = LphsBin ~ scaled_Diam,
       family = "binomial", data =subset(d, Species %in% "Alouatta palliata"))
summary(r4)

r5<-glm(formula = LphsBin ~ scaled_Diam,
       family = "binomial", data =subset(d, Species %in% "Alouatta seniculus"))
summary(r5)

r6<-glm(formula = LphsBin ~ scaled_Diam,
       family = "binomial", data =subset(d, Species %in% "Pithecia aequatorialis"))
summary(r6)

r7<-glm(formula = LphsBin ~ scaled_Diam,
       family = "binomial", data =subset(d, Species %in% "Callicebus discolor"))
summary(r7)

r8<-glm(formula = LphsBin ~ scaled_Diam,
       family = "binomial", data =subset(d, Species %in% "Cebus capucinus"))
summary(r8)

r9<-glm(formula = LphsBin ~ scaled_Diam,
       family = "binomial", data =subset(d, Species %in% "Saimiri sciureus"))
summary(r9)
