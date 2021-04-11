library(lme4)
library(broom)
library(tidyverse)

d<-read_csv("data.csv")
head(d)

r <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = d)
anova(r)

r1 <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = subset(d, Species %in% "Ateles belzebuth"))
summary(r1)

r2 <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = subset(d, Species %in% "Ateles geoffroyi"))
summary(r2)

r3 <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = subset(d, Species %in% "Lagothrix lagotricha"))
summary(r3)

r4 <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = subset(d, Species %in% "Alouatta palliata"))
summary(r4)

r5 <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = subset(d, Species %in% "Alouatta seniculus"))
summary(r5)

r6 <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = subset(d, Species %in% "Pithecia aequatorialis"))
summary(r6)

r7 <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = subset(d, Species %in% "Callicebus discolor"))
summary(r7)

r8 <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = subset(d, Species %in% "Cebus capucinus"))
summary(r8)

r9 <- lmer(MeanLphsMS ~ scaled_Diam + (1|MovClip), data = subset(d, Species %in% "Saimiri sciureus"))
summary(r9)

