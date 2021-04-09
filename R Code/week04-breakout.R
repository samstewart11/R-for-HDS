#week04 - breakout activity
dat = read.csv("data/framinghamFirst.csv",header=TRUE, 
               na.strings=".",stringsAsFactors=FALSE)
dat$BMIGroups = cut(dat$BMI,breaks=c(0,18.5,25,30,Inf),
                    labels=c("Underweight","Normal","Overweight","Obese"))
dat$SEX = factor(dat$SEX,levels=1:2,labels=c("Male","Female"))
dat$DIABETES = factor(dat$DIABETES,levels=0:1,labels=c("No Diabetes","Diabetes"))
dat$HYPERTEN = factor(dat$HYPERTEN,levels=0:1,labels=c("Normotensive","Hypertensive"))
dat$CURSMOKE = factor(dat$CURSMOKE,levels=0:1,labels=c("Non-Smoker","Smoker"))

# Is there a difference in glucose levels between smokers and non-smokers?
t.test01 = t.test(GLUCOSE~CURSMOKE,data=dat)
mod.lm01 = lm(GLUCOSE~CURSMOKE,data=dat)

# Is the difference still present after controlling for sex?
mod.lm02 = lm(GLUCOSE~CURSMOKE+SEX,data=dat)

# Is there an interaction between smoking and sex?
mod.lm03 = lm(GLUCOSE~CURSMOKE*SEX             ,data=dat)
mod.lm03 = lm(GLUCOSE~CURSMOKE+SEX+CURSMOKE*SEX,data=dat)

# Is there an effect of smoking on diabetes status?
tab01 = table(dat$CURSMOKE,dat$DIABETES)
chi.test01 = chisq.test(tab01)
mod.log01 = glm(DIABETES~CURSMOKE,data=dat,family=binomial)

# Is the effect of smoking on diabetes present after controlling for sex?
mod.log02 = glm(DIABETES~CURSMOKE+SEX,data=dat,family=binomial)
