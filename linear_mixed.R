library(lme4)
library(broom)
library(tidyverse)
library(lmerTest)


d<-read_csv("data.csv")
head(d)

r <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
            V_1 + V_2 + V_3 + V_4 + (1|MovClip), data = d)
anova(r)


r1 <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
             (1|MovClip), data = subset(d, Species %in% "Ateles belzebuth"))
anova(r1)

r2 <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
             (1|MovClip), data = subset(d, Species %in% "Ateles geoffroyi"))
anova(r2)

r3 <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
             (1|MovClip), data = subset(d, Species %in% "Lagothrix lagotricha"))
anova(r3)

r4 <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
             (1|MovClip), data = subset(d, Species %in% "Alouatta palliata"))
anova(r4)

r5 <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
             (1|MovClip), data = subset(d, Species %in% "Alouatta seniculus"))
anova(r5)

r6 <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
             (1|MovClip), data = subset(d, Species %in% "Pithecia aequatorialis"))
anova(r6)

r7 <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
             (1|MovClip), data = subset(d, Species %in% "Callicebus discolor"))
anova(r7)

r8 <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
             (1|MovClip), data = subset(d, Species %in% "Cebus capucinus"))
anova(r8)

r9 <- lmer(MeanLphsMS ~ scaled_Diam + sine_Orient + cos_Orient + sqrt_relSpeed +
             (1|MovClip), data = subset(d, Species %in% "Saimiri sciureus"))
anova(r9)

