## ----setup, include=FALSE-------------------------------------------------------------

library(Hmisc)
library(kableExtra)
library(dplyr)
library(tidyr)


knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE, fig.width=8, fig.height=5)
#This option should force knitr to use the project workng directory
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

options(full_width = FALSE)
options(knitr.kable.NA = '')



## ----readFram-------------------------------------------------------------------------

dat = read.csv("data/framinghamFirst.csv",header=TRUE, 
               na.strings=".",stringsAsFactors=FALSE)
dat$BMIGroups = cut(dat$BMI,breaks=c(0,18.5,25,30,Inf),
                    labels=c("Underweight","Normal","Overweight","Obese"))
dat$SEX = factor(dat$SEX,levels=1:2,labels=c("Male","Female"))
dat$DIABETES = factor(dat$DIABETES,levels=0:1,labels=c("No Diabetes","Diabetes"))
dat$HYPERTEN = factor(dat$HYPERTEN,levels=0:1,labels=c("Normotensive","Hypertensive"))



## ----boxplotFormulaExample------------------------------------------------------------

boxplot(SYSBP~SEX+DIABETES,data=dat,col=2:3)
boxplot(SYSBP~DIABETES+SEX,data=dat,col=2:3)



## ----t.test.examples01----------------------------------------------------------------

#testing Systolic blood pressure against a null of 0 
t.test(dat$SYSBP)
#testing against a null of 130
t.test(dat$SYSBP,mu=130)
#saving the value and extracting the components
tTest01 = t.test(dat$SYSBP,mu=130)
tTest01
names(tTest01)
tTest01$statistic
tTest01$p.value
tTest01$estimate
tTest01$conf.int




## ----t.test.examples02----------------------------------------------------------------
#testing systolic against diastolic BP
t.test(dat$SYSBP,dat$DIABP)
#testing systolic between sexes
t.test(dat$SYSBP~dat$SEX)
#almost all functions that take equations will let you submit a dataset
t.test(SYSBP~SEX,data=dat)

#we can extract the BPs using the tapply function
SBP.sex = tapply(dat$SYSBP,dat$SEX,c)
t.test(SBP.sex$Male,SBP.sex$Female)


## ----tapply.ex------------------------------------------------------------------------

#get the average SYSBP for each value of SEX
tapply(dat$SYSBP,dat$SEX,mean)
#get the average for each BMI group
tapply(dat$SYSBP,dat$BMIGroups,mean)
#the functions don't need to be numeric
par(mfrow=c(2,2))
temp = tapply(dat$SYSBP,dat$BMIGroups,hist)#this doesn't work well for titles



## ----chisq01--------------------------------------------------------------------------

tab01 = table(dat$SEX,dat$HYPERTEN)
chi01 = chisq.test(tab01,correct=FALSE)#I think this matches STATA
chi01 = chisq.test(tab01)#with the Yate's correction
chi01
names(chi01)
chi01$statistic
chi01$p.value

#this also works, but no table
chisq.test(dat$SEX,dat$HYPERTEN)



## ----fisher01-------------------------------------------------------------------------
fisher.test(tab01)
fish01 = fisher.test(tab01)
names(fish01)
fish01$estimate
fish01$conf.int


## ----epiR-----------------------------------------------------------------------------

library(epiR)
test01 = epi.2by2(tab01)
test01
names(test01)
test01$res$RR.crude.wald



## ----epiR02---------------------------------------------------------------------------

tab01#wrong arrangement of columns
tab01 = tab01[,2:1]#flip the columns
tab01
epi.2by2(tab01)



## ----lm01-----------------------------------------------------------------------------

mod.lm01 = lm(SYSBP~DIABP,data=dat)
mod.lm01
summary(mod.lm01)



## ----lm02-----------------------------------------------------------------------------

names(mod.lm01)
mod.lm01$coeff
sum = summary(mod.lm01)
sum$coefficients
sum$r.squared
sum$fstatistic



## ----lm.plot01,fig.height=8,fig.width=8-----------------------------------------------

par(mfrow=c(2,2))
plot(mod.lm01)



## ----lm.plot02, fig.height=4,fig.width=8----------------------------------------------
par(mfrow=c(1,2))
plot(mod.lm01,which=c(1,2))


## ----lm.multi01-----------------------------------------------------------------------

library(car)
mod.lm02 = lm(SYSBP~DIABP+AGE+SEX+BMIGroups+DIABETES+CURSMOKE,data=dat)
car::Anova(mod.lm02)
summary(mod.lm02)



## ----lm.multi02-----------------------------------------------------------------------

confint(mod.lm02)



## ----postHoc01------------------------------------------------------------------------

mod.lm03 = lm(SYSBP~BMIGroups,data=dat)
#first convert to aov
aov03 = aov(mod.lm03)
#TukeyHSD performs the post-hoc CIs using the Tukey Honest Significant Difference
TukeyHSD(aov03)



## ----multcomp01-----------------------------------------------------------------------

library(multcomp)
CIs = glht(mod.lm03,linfct=mcp(BMIGroups='Tukey'))
summary(CIs,test=adjusted(type='bonferroni'))



## ----logistic01-----------------------------------------------------------------------

mod.log01 = glm(CURSMOKE~GLUCOSE,data=dat,family=binomial)
#THIS IS THE WRONG MODEL SPEC
mod.log01.wrong = glm(CURSMOKE~GLUCOSE,data=dat)
summary(mod.log01)
summary(mod.log01.wrong)



## ----logistic02-----------------------------------------------------------------------

mod.log02 = glm(DIABETES~GLUCOSE,data=dat,family=binomial)
#This will throw an error since DIABETES is a factor variable
#mod.log02 = glm(DIABETES~GLUCOSE,data=dat)
mod.log02
summary(mod.log02)



## ----logsitic.OR01--------------------------------------------------------------------

mod.log03 = glm(CURSMOKE~SEX+SYSBP+GLUCOSE+BMIGroups,data=dat,family=binomial)
#extract the coefficients
coef = mod.log03$coefficients
#get the CIs on the coefficients
ci = confint(mod.log03)
#tie them together in a table
out = cbind(OR=coef,ci)
#exponentiate them to get the ORs
out01 = exp(out)

#or in one line
out02 = exp(cbind(OR=mod.log03$coefficients,confint(mod.log03)))



## ----logistic.OR02--------------------------------------------------------------------

getOR = function(mod){
  out = exp(cbind(OR=mod$coefficients,confint(mod)))
  return(out)
}
out03 = getOR(mod.log03)



## ----modelOR.code---------------------------------------------------------------------

modelOR = function(model,alpha=0.05,pvalue=FALSE){
  z = qnorm(1-alpha/2,0,1)
  lev = model$xlevels
  lab = names(model$model)[-1]
  dat = model$data
  lreg.coeffs <- coef(summary(model))
  k=2
  rowNames = matrix(rep(vector(length=dim(lreg.coeffs)[1]),4),ncol=4)
  rowNames[1,] = c('(Intercept)','','','')
  for(i in 1:length(lab)){
    if(lab[i]%in%names(lev)){
      # categorical variable
      for(j in 2:length(lev[[(1:length(lev))[names(lev)==lab[i]]]])){
        rowNames[k,] = c(paste(lab[i],":"),lev[[(1:length(lev))[names(lev)==lab[i]]]][j],'vs.',lev[[(1:length(lev))[names(lev)==lab[i]]]][1])
        k=k+1        
      }
    }
    else{
      rowNames[k,] = c(paste(lab[i],':'),'1','unit','increase')
      k=k+1      
    }
  }
  lci <- exp(lreg.coeffs[ ,1] - z * lreg.coeffs[ ,2])
  or <- exp(lreg.coeffs[ ,1])
  uci <- exp(lreg.coeffs[ ,1] + z * lreg.coeffs[ ,2])
  lreg.or <- data.frame(cbind(lci, or, uci))
  orNames = vector(length=dim(rowNames)[1])
  for(i in 1:dim(lreg.or)[1]){
    if(lreg.or[i,2] < 1){
      # invert the odds ratio
      tempLci = 1/lreg.or[i,3]
      tempOR = 1/lreg.or[i,2]
      tempUci = 1/lreg.or[i,1]
      lreg.or[i,] = c(tempLci,tempOR,tempUci)
      # fix the row names
      if(rowNames[i,3]=='vs.'){
        temp = rowNames[i,2]
        rowNames[i,2] = rowNames[i,4]
        rowNames[i,4] = temp
      }
      else{
        rowNames[i,4] = 'decrease'
      }
    }
    orNames[i] = paste(rowNames[i,1],rowNames[i,2],rowNames[i,3],rowNames[i,4])
  }
  names(lreg.or) = c("Lower CI","OR","Upper CI")
  if(pvalue){
    lreg.or[,4] = (summary(model))$coefficients[,4]
    names(lreg.or)[4] = "p-value"
  }
  #The function doesn't handle interaction models well, even though I usually only want to first order ORs, so I need to fix the rowNames
  if(sum(duplicated(orNames))>0){
    orNames[which(duplicated(orNames))] = LETTERS[1:length(which(duplicated(orNames)))]
  }
  row.names(lreg.or) = orNames
  if(pvalue){
    
  }
  return(lreg.or)
}


