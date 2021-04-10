library(tidyverse)
library(cowplot)

d<-read_csv("data.csv")
head(d)

#creating plots for relative substrate diameter
p1<-ggplot(subset(d, Species %in% "Alouatta palliata"), 
  aes(x = scaled_Diam, y = MeanLphsMS)) +scale_color_manual(values="orangered4")+
  geom_point(aes(color=Species), alpha=.5)+geom_smooth(method="lm",formula=y~x, aes(color=Species), fill="orangered4")+
  labs(y= "Limb Phase", x = "Relative Substrate Diameter")+ylim(0.00, 1.00)+xlim(0.0,15.0)+
  geom_hline(yintercept=.5, linetype="dashed", size=1)+
  annotate(geom = "text", x = 0, y = .75, label = "DS", color = "black", angle = 90, fontface=2)+
  annotate(geom = "text", x = 0, y = .25, label = "LS", color = "black", angle = 90, fontface=2)+
  theme(axis.title.x=element_blank(),legend.position=c(.75,.92),
  legend.title=element_blank(), legend.background = element_rect(fill=NA),legend.text=element_text(face="italic"))+
  guides(color=guide_legend(override.aes=list(fill="gray")))
p1<-p1+theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
             panel.grid=element_line("gray92"), axis.title.y=element_text(face="bold"))

p2<-ggplot(subset(d, Species %in% "Alouatta seniculus"), 
  aes(x = scaled_Diam, y = MeanLphsMS)) +scale_color_manual(values="darkorange")+
  geom_point(aes(color=Species), alpha=.5)+geom_smooth(method="lm",formula=y~x, aes(color=Species), fill="darkorange")+
  labs(y= "Limb Phase", x = "Relative Substrate Diameter")+ylim(0.00, 1.00)+xlim(0.0,7.0)+
  geom_hline(yintercept=.5, linetype="dashed", size=1)+
  annotate(geom = "text", x = 0, y = .75, label = "DS", color = "black", angle = 90, fontface=2)+
  annotate(geom = "text", x = 0, y = .25, label = "LS", color = "black", angle = 90, fontface=2)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position=c(.75,.92),
  legend.title=element_blank(), legend.background = element_rect(fill=NA),legend.text=element_text(face="italic"))+
  guides(color=guide_legend(override.aes=list(fill="gray")))
p2<-p2+theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
             panel.grid=element_line("gray92"))

p3<-ggplot(subset(d, Species %in% "Ateles belzebuth"), 
  aes(x = scaled_Diam, y = MeanLphsMS)) +scale_color_manual(values="darkblue")+
  geom_point(aes(color=Species), alpha=.5)+geom_smooth(method="lm",formula=y~x, aes(color=Species), fill="darkblue")+
  labs(y= "Limb Phase", x = "Relative Substrate Diameter")+ylim(0.00, 1.00)+xlim(0.0,9.0)+
  geom_hline(yintercept=.5, linetype="dashed", size=1)+
  annotate(geom = "text", x = 0, y = .75, label = "DS", color = "black", angle = 90, fontface=2)+
  annotate(geom = "text", x = 0, y = .25, label = "LS", color = "black", angle = 90, fontface=2)+
  theme(axis.title.x=element_blank(),legend.position=c(.75,.92),
  legend.title=element_blank(), legend.background = element_rect(fill=NA),legend.text=element_text(face="italic"))+
  guides(color=guide_legend(override.aes=list(fill="gray")))
p3<-p3+theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
             panel.grid=element_line("gray92"), axis.title.y=element_text(face="bold"))

p4<-ggplot(subset(d, Species %in% "Ateles geoffroyi"), 
  aes(x = scaled_Diam, y = MeanLphsMS)) +scale_color_manual(values="dodgerblue3")+
  geom_point(aes(color=Species), alpha=.5)+geom_smooth(method="lm",formula=y~x,aes(color=Species), fill="dodgerblue3")+
  labs(y= "Limb Phase", x = "Relative Substrate Diameter")+ylim(0.00, 1.00)+xlim(0.0,11.0)+
  geom_hline(yintercept=.5, linetype="dashed", size=1)+
  annotate(geom = "text", x = 0, y = .75, label = "DS", color = "black", angle = 90, fontface=2)+
  annotate(geom = "text", x = 0, y = .25, label = "LS", color = "black", angle = 90, fontface=2)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position=c(.75,.92),
  legend.title=element_blank(), legend.background = element_rect(fill=NA),legend.text=element_text(face="italic"))+
  guides(color=guide_legend(override.aes=list(fill="gray")))
p4<-p4+theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
             panel.grid=element_line("gray92"))

p5<-ggplot(subset(d, Species %in% "Callicebus discolor"), 
  aes(x = scaled_Diam, y = MeanLphsMS)) +scale_color_manual(values="tomato")+
  geom_point(aes(color=Species), alpha=.5)+geom_smooth(method="lm",formula=y~x, aes(color=Species), fill="tomato")+
  labs(y= "Limb Phase", x = "Relative Substrate Diameter")+ylim(0.00, 1.00)+xlim(0.0,6.0)+
  geom_hline(yintercept=.5, linetype="dashed", size=1)+
  annotate(geom = "text", x = 0, y = .75, label = "DS", color = "black", angle = 90, fontface=2)+
  annotate(geom = "text", x = 0, y = .25, label = "LS", color = "black", angle = 90, fontface=2)+
  theme(axis.title.y=element_blank(),legend.position=c(.75,.92),
  legend.title=element_blank(), legend.background = element_rect(fill=NA),legend.text=element_text(face="italic"))+
  guides(color=guide_legend(override.aes=list(fill="gray")))
p5<-p5+theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
             panel.grid=element_line("gray92"),axis.title.x=element_text(face="bold"))

p6<-ggplot(subset(d, Species %in% "Cebus capucinus"), 
  aes(x = scaled_Diam, y = MeanLphsMS)) +scale_color_manual(values="purple4")+
  geom_point(aes(color=Species), alpha=.5)+geom_smooth(method="lm",formula=y~x, aes(color=Species), fill="purple4")+
  labs(y= "Limb Phase", x = "Relative Substrate Diameter")+ylim(0.00, 1.00)+xlim(0.0,12.5)+
  geom_hline(yintercept=.5, linetype="dashed", size=1)+
  annotate(geom = "text", x = 0, y = .75, label = "DS", color = "black", angle = 90, fontface=2)+
  annotate(geom = "text", x = 0, y = .25, label = "LS", color = "black", angle = 90, fontface=2)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position=c(.75,.92),
  legend.title=element_blank(), legend.background = element_rect(fill=NA),legend.text=element_text(face="italic"))+
  guides(color=guide_legend(override.aes=list(fill="gray")))
p6<-p6+theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
             panel.grid=element_line("gray92"))

p7<-ggplot(subset(d, Species %in% "Lagothrix lagotricha"), 
  aes(x = scaled_Diam, y = MeanLphsMS)) +scale_color_manual(values="mediumseagreen")+
  geom_point(aes(color=Species), alpha=.5)+geom_smooth(method="lm",formula=y~x, aes(color=Species), fill="mediumseagreen")+
  labs(y= "Limb Phase", x = "Relative Substrate Diameter")+ylim(0.00, 1.00)+xlim(0.0,12.5)+
  geom_hline(yintercept=.5, linetype="dashed", size=1)+
  annotate(geom = "text", x = 0, y = .75, label = "DS", color = "black", angle = 90, fontface=2)+
  annotate(geom = "text", x = 0, y = .25, label = "LS", color = "black", angle = 90, fontface=2)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position=c(.75,.92),
  legend.title=element_blank(), legend.background = element_rect(fill=NA),legend.text=element_text(face="italic"))+
  guides(color=guide_legend(override.aes=list(fill="gray")))
p7<-p7+theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
             panel.grid=element_line("gray92"))

p8<-ggplot(subset(d, Species %in% "Pithecia aequatorialis"), 
  aes(x = scaled_Diam, y = MeanLphsMS)) +scale_color_manual(values="darkred")+
  geom_point(aes(color=Species), alpha=.5)+geom_smooth(method="lm",formula=y~x, aes(color=Species), fill="darkred")+
  labs(y= "Limb Phase", x = "Relative Substrate Diameter")+ylim(0.00, 1.00)+xlim(0.0,6.0)+
  geom_hline(yintercept=.5, linetype="dashed", size=1)+
  annotate(geom = "text", x = 0, y = .75, label = "DS", color = "black", angle = 90, fontface=2)+
  annotate(geom = "text", x = 0, y = .25, label = "LS", color = "black", angle = 90, fontface=2)+
  theme(axis.title.y=element_blank(),legend.position=c(.75,.92),legend.title=element_blank(),
  legend.background = element_rect(fill=NA),legend.text=element_text(face="italic"))+
  guides(color=guide_legend(override.aes=list(fill="gray")))
p8<-p8+theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
             panel.grid=element_line("gray92"),axis.title.x=element_text(face="bold"))

p9<-ggplot(subset(d, Species %in% "Saimiri sciureus"), 
  aes(x = scaled_Diam, y = MeanLphsMS)) +scale_color_manual(values="mediumpurple")+
  geom_point(aes(color=Species),alpha=.5)+geom_smooth(method="lm",formula=y~x,aes(color=Species), fill="mediumpurple")+
  labs(y= "Limb Phase", x = "Relative Substrate Diameter")+ylim(0.00, 1.00)+xlim(0.0,12.5)+
  geom_hline(yintercept=.5, linetype="dashed", size=1)+
  annotate(geom = "text", x = 0, y = .75, label = "DS", color = "black", angle = 90, fontface=2)+
  annotate(geom = "text", x = 0, y = .25, label = "LS", color = "black", angle = 90, fontface=2)+
  theme(legend.position=c(.75,.92),legend.title=element_blank(),legend.background = element_rect(fill=NA),
  legend.text=element_text(face="italic"))+guides(color=guide_legend(override.aes=list(fill="gray")))
p9<-p9+theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
  panel.grid=element_line("gray92"), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))
p9

#plotting in the order of the graphs in the paper
plot_grid(p3, p4, p7, p1, p2, p6, p9, p8, p5)

