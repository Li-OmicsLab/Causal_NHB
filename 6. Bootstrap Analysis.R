

#Bootstrap Analysis#
library(mediation)
library(Hmisc)
mydata=spss.get("D:/depression/unmarried.sav")
mydata$education <-factor (mydata$education)
mydata$drink <-factor (mydata$drink)
mydata$smoke <-factor (mydata$smoke)
mydata$diabetes <-factor (mydata$diabetes)
mydata$hypertension <-factor (mydata$hypertension)
mydata$heart <-factor (mydata$heart)
mydata$cancer <-factor (mydata$cancer)
mydata$stroke <-factor (mydata$stroke)
ACME <- data.frame(Estimate=rep(NA,500),
                   CI_lower=rep(NA,500),
                   CI_upper=rep(NA,500))
ADE <- data.frame(Estimate=rep(NA,500),
                  CI_lower=rep(NA,500),
                  CI_upper=rep(NA,500))
Prop <- data.frame(Estimate=rep(NA,500),
                   CI_lower=rep(NA,500),
                   CI_upper=rep(NA,500))
for(i in 1:500){#bootstrap resampling = 50% or 80% of total samples#
  sample1=sample(nrow(mydata),3442,replace=F)
  data1=mydata[sample1,]
  #establishing a causal model between marriage and alcohol drinking#
  medModel<-glm(drink~marry+age+sex+education+smoke+income+bmi+diabetes+hypertension+heart+cancer+stroke,
                family=binomial(link = "probit"),
                control=list(maxit=1000),
                data=data1)
  #establishing a causal model between marriage,alcohol drinking, and depressive symptoms#
  outModel<-glm(depression~marry*drink+age+sex+education+smoke+income+bmi+diabetes+hypertension+heart+cancer+stroke,
                
                control=list(maxit=1000),family=binomial(link = "probit"), 
                data=data1)
  
  med<-mediate(model.m =medModel,model.y=outModel,treat = 'marry',
               mediator = 'drink', weights = weight, data=data1)
  s <- summary(med)
  ACME[i,] <- c(s$d.avg,s$d.avg.ci)
  ADE[i,] <- c(s$z.avg,s$z.avg.ci)
  Prop[i,] <- c(s$n.avg,s$n.avg.ci)
}
write.csv(cbind.data.frame(ACME,ADE,Prop),file = "D:/depression/unmarried.csv",
          row.names = F,quote = F)

library(mediation)
library(Hmisc)
mydata=spss.get("D:/depression/unmarried.sav")
mydata$education <-factor (mydata$education)
mydata$drink <-factor (mydata$drink)
mydata$smoke <-factor (mydata$smoke)
mydata$diabetes <-factor (mydata$diabetes)
mydata$hypertension <-factor (mydata$hypertension)
mydata$heart <-factor (mydata$heart)
mydata$cancer <-factor (mydata$cancer)
mydata$stroke <-factor (mydata$stroke)
ACME <- data.frame(Estimate=rep(NA,500),
                   CI_lower=rep(NA,500),
                   CI_upper=rep(NA,500))
ADE <- data.frame(Estimate=rep(NA,500),
                  CI_lower=rep(NA,500),
                  CI_upper=rep(NA,500))
Prop <- data.frame(Estimate=rep(NA,500),
                   CI_lower=rep(NA,500),
                   CI_upper=rep(NA,500))
for(i in 1:500){#bootstrap resampling = 50% or 80% of total samples#
  sample1=sample(nrow(mydata),3442,replace=F)
  data1=mydata[sample1,]
  #establishing a causal model between marriage and smoking#
  medModel<-glm(smoke~marry+age+sex+education+drink+income+bmi+diabetes+hypertension+heart+cancer+stroke,
                family=binomial(link = "probit"),
                control=list(maxit=1000),
                data=data1)
  #establishing a causal model between marriage,smoking, and depressive symptoms#
  outModel<-glm(depression~marry*smoke+age+sex+education+drink+income+bmi+diabetes+hypertension+heart+cancer+stroke,
                
                control=list(maxit=1000),family=binomial(link = "probit"), 
                data=data1)
  
  med<-mediate(model.m =medModel,model.y=outModel,treat = 'marry',
               mediator = 'smoke', weights = weight, data=data1)
  s <- summary(med)
  ACME[i,] <- c(s$d.avg,s$d.avg.ci)
  ADE[i,] <- c(s$z.avg,s$z.avg.ci)
  Prop[i,] <- c(s$n.avg,s$n.avg.ci)
}
write.csv(cbind.data.frame(ACME,ADE,Prop),file = "D:/depression/unmarried.csv",
          row.names = F,quote = F)

