## ----setup, include=FALSE----------------------------------------------------------

knitr::opts_chunk$set(tidy = TRUE, echo = TRUE, message=FALSE, warning=FALSE, fig.width=8, fig.height=5)

#This option should force knitr to use the project workng directory
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

options(full_width = FALSE)
options(knitr.kable.NA = '')



## ----readFram----------------------------------------------------------------------
library(Hmisc)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(boomer)


dat = read.csv("data/framinghamFirst.csv",header=TRUE, 
               na.strings=".",stringsAsFactors=FALSE)
dat$BMIGroups = cut(dat$BMI,breaks=c(0,18.5,25,30,Inf),
                    labels=c("Underweight","Normal","Overweight","Obese"))
dat$SEX = factor(dat$SEX,levels=1:2,labels=c("Male","Female"))
dat$DIABETES = factor(dat$DIABETES,levels=0:1,labels=c("No Diabetes","Diabetes"))
dat$HYPERTEN = factor(dat$HYPERTEN,levels=0:1,labels=c("Normotensive","Hypertensive"))

# I want a dataset that's a bit easier to see, so I'll use dat.reduced on occasion
# in this session
set.seed(11)
dat.reduced = dat %>% sample_n(200)
  


## ----ggplot01----------------------------------------------------------------------

ggplot(dat.reduced)+
  geom_point(aes(x=SYSBP,y=DIABP))



## ----ggplot01a, eval=FALSE---------------------------------------------------------
## 
## ggplot(dat.reduced,aes(x=SYSBP,y=DIABP))+
##   geom_point()
## 


## ----ggplot02----------------------------------------------------------------------

dat.reduced%>%
  ggplot(aes(x=SYSBP,y=DIABP))+
  geom_point(aes(color=SEX))



## ----ggplot03----------------------------------------------------------------------

dat.reduced%>%
  ggplot(aes(x=SYSBP,y=DIABP,color=SEX,group=SEX))+
  geom_point()+
  geom_smooth()



## ----ggplot04----------------------------------------------------------------------

dat.reduced%>%
  ggplot(aes(x=SYSBP,y=DIABP,color=SEX,group=SEX))+
  geom_point()+
  geom_smooth()+
  labs(x="Systolic Blood Pressure",
       y='Diastolic Blood Pressure',
       title='BP relationship, stratified by SEX', 
       color='')



## ----geom_point01------------------------------------------------------------------

dat.reduced%>%
  ggplot(aes(x=SYSBP,y=DIABP,color=SEX,group=SEX))+
  geom_point(size=3,shape=17)

dat.reduced%>%
  ggplot(aes(x=SYSBP,y=DIABP,color=SEX,group=SEX))+
  geom_point(aes(size=BMIGroups,shape=SEX))



## ----geom_smooth01-----------------------------------------------------------------

dat.reduced%>%
  ggplot(aes(x=SYSBP,y=DIABP,color=SEX,group=SEX))+
  geom_point(aes(shape=SEX,size=2))+
  guides(size=FALSE)+
  geom_smooth()



## ----geom_smooth02-----------------------------------------------------------------

dat.reduced%>%
  ggplot(aes(x=SYSBP,y=DIABP,color=SEX,group=SEX))+
  geom_point(aes(shape=SEX,size=2))+
  guides(size=FALSE)+
  geom_smooth(se=FALSE,method='lm',show.legend=FALSE)



## ----box01-------------------------------------------------------------------------

dat %>%
ggplot(aes(x=BMIGroups,y=TOTCHOL))+
  geom_boxplot(aes(fill=BMIGroups))

dat %>%
ggplot(aes(x=BMIGroups,y=TOTCHOL))+
  geom_boxplot(aes(colour=BMIGroups))



## ----violin01----------------------------------------------------------------------

dat %>%
  ggplot(aes(x=BMIGroups,y=TOTCHOL))+
  geom_violin(aes(fill=BMIGroups),
              draw_quantiles=seq(0,1,by=0.25))



## ----hist01------------------------------------------------------------------------

dat %>%
ggplot(aes(x=TOTCHOL))+
  geom_histogram(aes(group=SEX,fill=SEX))



## ----hist02------------------------------------------------------------------------

dat %>%
ggplot(aes(x=BMI,group=SEX,fill=SEX))+
  geom_histogram(position='identity',alpha=0.5)



## ----bar01-------------------------------------------------------------------------

dat %>%
  ggplot(aes(x=BMIGroups,group=SEX,fill=SEX))+
  geom_bar()

dat %>%
  ggplot(aes(x=BMIGroups,group=SEX,fill=SEX))+
  geom_bar(position='dodge')


