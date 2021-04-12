
d<-read_csv("data.csv")
d$LphsBin<-as.numeric(d$MeanLphsMS > .5)
head(d)

range(d$SbstDiam)
range(d$SbstOrient)

#the only data provided is on recordings of symmetrical gaits, so 
#I cannot reproduce their descriptive stats based on symmetrical vs. 
#asymmetrical gaits
