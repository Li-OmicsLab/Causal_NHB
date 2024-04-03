#Moderator Analyses#
#install.packages("metafor")#
library(metafor)
library(Hmisc)
mydata=spss.get("D:/meta/moderator/unmarried.sav")
print(mydata, row.names=FALSE)
dat = escalc(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data =mydata,
             append = TRUE)
args(rma)
res = rma(ai = tpos, bi = tneg, ci = cpos, di = cneg, data = mydata, measure = "OR")
res
#Note: Enter the value of 'exp'#
exp(c(0.5029, 0.3216, 0.6842))
confint(res)
forest(res, slab = paste(dat$Survey,  sep = ", "),
       xlim = c(-14, 7), at = log(c(0.1,1,3,6)), atransf = exp,
       ilab = cbind(dat$tpos, dat$tneg, dat$cpos, dat$cneg),
       ilab.xpos = c(-9.5, -8, -6, -4.5), cex = 0.75)
op = par(cex = 0.75, font = 2)
text(c(-9.5, -8, -6, -4.5), 17, c("DPN", "Total", "DPN",  "Total"))
text(c(-8.75, -5.25), 18, c("All others", "Married"))
text(-14, 17, "Dataset", pos = 4)
text(6, 17, "OR [95% CI]", pos = 2)

#The Moderator is a two-category variable#
rma(yi, vi, data = dat, subset = (group == "1"))
rma(yi, vi, data = dat, subset = (group == "2"))
rma(yi, vi, mods = ~factor(group) - 1, data = dat)
rma(yi, vi, mods = ~ relevel(factor(group), ref = "1"), data = dat)

#The Moderator is a three-category variable#
rma(yi, vi, data = dat, subset = (group == "1"))
rma(yi, vi, data = dat, subset = (group == "2"))
rma(yi, vi, data = dat, subset = (group == "3"))
rma(yi, vi, mods = ~factor(group) - 1, data = dat)
rma(yi, vi, mods = ~ relevel(factor(group), ref = "1"), data = dat)

