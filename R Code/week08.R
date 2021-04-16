## ----setup, include=FALSE------------------------------------------

knitr::opts_chunk$set(tidy = TRUE, echo = TRUE, message=FALSE, warning=FALSE, fig.width=8, fig.height=5)

#This option should force knitr to use the project workng directory
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

options(full_width = FALSE)
options(knitr.kable.NA = '')



## ----readFram------------------------------------------------------
library(Hmisc)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(boomer)
library(survival)
library(survminer)
library(haven)
library(lme4)
library(mlmRev)

dat = read.csv("data/framinghamFirst.csv",header=TRUE, 
               na.strings=".",stringsAsFactors=FALSE)
dat$BMIGroups = cut(dat$BMI,breaks=c(0,18.5,25,30,Inf),
                    labels=c("Underweight","Normal","Overweight","Obese"))
dat$SEX = factor(dat$SEX,levels=1:2,labels=c("Male","Female"))
dat$DIABETES = factor(dat$DIABETES,levels=0:1,labels=c("No Diabetes","Diabetes"))
dat$HYPERTEN = factor(dat$HYPERTEN,levels=0:1,labels=c("Normotensive","Hypertensive"))

#for the survival analysis
dat.lung = lung %>% tibble %>%
  mutate(
    sex = factor(sex,levels=1:2,labels=c("M","F")),
    ageGroup = cut(age,breaks=c(0,50,60,70,100)),
    ph.ecog = factor(ph.ecog)
  )

#for the multilevel models
imm = read_dta("https://stats.idre.ucla.edu/stat/examples/imm/imm10.dta")

dat.imm = imm %>% 
  mutate(homework.fact = factor(homework))



## ----km01----------------------------------------------------------

km.lung00 = survfit(Surv(time,status)~1,data=dat.lung)
km.lung00
summary(km.lung00)

km.lung01 = survfit(Surv(time,status)~sex,data=dat.lung)
km.lung01
summary(km.lung01)



## ----km02----------------------------------------------------------

plot(km.lung00)
plot(km.lung01,col=2:3,xlab='Days',ylab='Survival Probabilty',main='Lung cancer survival rate by Sex')
legend("topright",fill=2:3,legend=c("Male","Female"))


## ----km03,fig.height=8,fig.width=8---------------------------------
ggsurvplot(km.lung00)

ggsurvplot(km.lung01,
           ggtheme=theme_grey(),
           conf.int=TRUE,
           pval=TRUE,
           risk.table=TRUE)+
  labs(x="Days",Strata="")



## ----coxph01-------------------------------------------------------

#simple model predicting survival with sex
mod.lung01 = coxph(Surv(time,status)~sex,data=dat.lung)
#Anova evaluates each variables
car::Anova(mod.lung01)
#getting the coefficient table
summary(mod.lung01)

#full model predicting survival with sex, ageGroup and ECOG score
mod.lung02 = coxph(Surv(time,status)~sex+ageGroup+ph.ecog,data=dat.lung)
car::Anova(mod.lung02)
summary(mod.lung02)



## ----coxph02-------------------------------------------------------

#in multiple steps like a sane person
#a) extract the coefficients
coef = summary(mod.lung02)$coefficients %>% as.data.frame() %>%
  rownames_to_column()
#b) extract the confidence intervals
ci = summary(mod.lung02)$conf.int %>% as.data.frame %>%
              rownames_to_column()
#c) join the two, and reduce to useful columns
out01 = left_join(coef,ci) %>% 
  select(rowname,'exp(coef)','lower .95','upper .95','Pr(>|z|)') %>%
  rename(RR=`exp(coef)`,
         variable = rowname)
#d) print the columns
out01 %>% kable(digits=c(0,3,2,2,4)) %>% kable_styling()

#OR
#do it all at once
summary(mod.lung02)$coefficients %>% as.data.frame %>% 
  rownames_to_column() %>%
  left_join(summary(mod.lung02)$conf.int %>% as.data.frame %>%
              rownames_to_column()
              ) %>%
  select(rowname,'exp(coef)','lower .95','upper .95','Pr(>|z|)') %>%
  rename(RR=`exp(coef)`,
         variable = rowname) %>% 
  kable(digits=c(0,3,2,2,4)) %>% kable_styling() %>%
  pack_rows("Sex",1,1)%>%
  pack_rows("Age Group",2,4)%>%
  pack_rows("ECOG Score",5,7)



## ----imm10.explore-------------------------------------------------

dat.imm %>%
  ggplot(aes(x=math))+
  geom_histogram()

#geom_jitter is like geom_point, but can scatter things a bit randomly, useful for overlapping datapoints
dat.imm %>%
  ggplot(aes(x=homework,y=math))+
  geom_violin(aes(group=homework,colour=homework.fact))+
#  geom_point(aes(colour=homework.fact))+
  geom_jitter(aes(colour=homework.fact),width=0.2,height=0)+
  geom_smooth()
  
dat.imm %>%
  ggplot(aes(x=math))+
  geom_histogram()+
  facet_wrap(~schid)

dat.imm %>%
  ggplot(aes(x=homework,y=math))+
  geom_point(aes(colour=homework.fact))+
  geom_smooth()+
  facet_wrap(~schid)



## ----imm.linear01--------------------------------------------------

lin.mods = by(dat.imm,dat.imm$schid,function(x){lm(math~homework,data=x)})



## ----imm.linear02--------------------------------------------------

t(sapply(lin.mods,coef))



## ----imm.lme01-----------------------------------------------------

mod.lme01 = lmer(math~homework+(homework|schid),data=dat.imm)
mod.lme01
car::Anova(mod.lme01)
summary(mod.lme01)



## ----imm.lme02-----------------------------------------------------

mod.lme02 = lmer(math~homework+public+(homework|schid),data=dat.imm)
mod.lme02
car::Anova(mod.lme02)
summary(mod.lme02)

mod.lme03 = lmer(math~homework*public+(homework|schid),data=dat.imm)
mod.lme03
car::Anova(mod.lme03)
summary(mod.lme03)


