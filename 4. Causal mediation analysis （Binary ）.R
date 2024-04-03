#Causal mediation analysis#
#install.packages('mediation')#
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

library(mediation)
#mediator:alcohol drinking#
#establishing a causal model between marriage and alcohol drinking#
medModel<-glm(drink~marry+age+sex+education+smoke+income+bmi+diabetes+hypertension+heart+cancer+stroke,
              family=binomial(link = "probit"),
              control=list(maxit=1000),
              data=mydata)
#establishing a causal model between marriage,alcohol drinking, and depressive symptoms#
outModel<-glm(depression~marry*drink+age+sex+education+smoke+income+bmi+diabetes+hypertension+heart+cancer+stroke,
              
              control=list(maxit=1000),family=binomial(link = "probit"), 
              data=mydata)

library(mediation)
med<-mediate(model.m =medModel,model.y=outModel,treat = 'marry',
             mediator = 'drink', weights = weight, data=mydata)
summary(med)


library(mediation)
#mediator:smoking#
#establishing a causal model between marriage and smoking#
medModel<-glm(smoke~marry+age+sex+education+drink+income+bmi+diabetes+hypertension+heart+cancer+stroke,
              family=binomial(link = "probit"),
              control=list(maxit=1000),
              data=mydata)
#establishing a causal model between marriage,smoking, and depressive symptoms#
outModel<-glm(depression~marry*smoke+age+sex+education+smoke+income+bmi+diabetes+hypertension+heart+cancer+stroke,
              
              control=list(maxit=1000),family=binomial(link = "probit"), 
              data=mydata)

library(mediation)
med<-mediate(model.m =medModel,model.y=outModel,treat = 'marry',
             mediator = 'smoke', weights = weight, data=mydata)
summary(med)