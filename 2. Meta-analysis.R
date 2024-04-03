#Meta-analysis#
#install.packages('Hmisc')#
#install.packages('meta')#
library(Hmisc)
mydata=spss.get("D:/meta/unmarried.sav")
library(meta)
metabin(event.e,n.e,event.c,n.c,data=mydata,sm="OR")
m<-metabin(event.e,n.e,event.c,n.c,data=mydata,sm="OR",
           studlab = paste(Survey))
forest(m,comb.fixed=FALSE, xlim = c(0.5,6), at=c(0.5,1, 3,6))
metainf(m,pooled="random")