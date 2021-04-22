## ----setup, include=FALSE---------------------------------------------

knitr::opts_chunk$set(tidy = TRUE, echo = TRUE, message=FALSE, warning=FALSE, fig.width=8, fig.height=5)

#This option should force knitr to use the project workng directory
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

options(full_width = FALSE)
options(knitr.kable.NA = '')



## ----readFram---------------------------------------------------------
library(Hmisc)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(boomer)
library(table1)
library(HSAUR)
library(ggpubr)
library(gee)

#GEE example data - if you don't want to load a package you can load a
#dataset from it like this (though you'll still need to download the package)
data("BtheB", package = "HSAUR")


## ----summaryData------------------------------------------------------

#make it a tibble for ease of printing, and add an ID column
BtheB = BtheB %>% tibble() %>%
  mutate(ID = 1:n()) %>%
  relocate(ID)
#make a long version of the bdi variables
BtheB.long = 
BtheB %>%
  pivot_longer(cols=-c(ID, drug,length,treatment),names_to='time',values_to='beck') %>%
  mutate(time=factor(gsub("bdi.","",time),levels=c("pre","2m","4m","6m","8m")))

#some functions use labels for better printing
label(BtheB$drug) = 'On depression medications'
label(BtheB$length) = 'Duration of depressive symptoms'

#table1 will give us a quick summary
table1(~.|treatment,data=BtheB)




## ----trajectories,fig.height=6,fig.width=7----------------------------

p1 = BtheB.long %>%
  ggplot(aes(x=time,y=beck,color=treatment))+
  geom_point()+
  geom_line(aes(group=ID))
  
p2 = BtheB.long %>%
  ggplot(aes(x=time,y=beck))+
  geom_point(colour=grey(0.75))+
  geom_line(aes(group=ID),colour=grey(0.75))+
  geom_smooth(aes(colour=treatment,group=treatment),se=FALSE)+
  theme_classic()
#this function from the ggpubr library is useful for combining ggplots
ggarrange(p1,p2,nrow=2)


## ----gee.data---------------------------------------------------------

#I'm going to move bdi.pre from an outcome measure to a predictor
BtheB.long.model = 
BtheB %>%
  pivot_longer(cols=-c(ID, drug,length,treatment,bdi.pre),
               names_to='time',values_to='bdi') %>%
  mutate(time=factor(gsub("bdi.","",time),levels=c("2m","4m","6m","8m")))%>%
  arrange(ID,time)%>%
  drop_na()

BtheB.long.model


## ----gee01------------------------------------------------------------
#to build a gee model we just need to specify the variable that 
#co-ordinates the groups (ID), the family, and the correlation structure
mod01 =  gee(bdi ~ bdi.pre + treatment + length + drug, 
             data = BtheB.long.model, id = ID, family = gaussian, corstr = "independence")
mod01
summary(mod01)


## ----gee.modelExtraction----------------------------------------------
#You need to get CIs and p-values yourself, so I built a function to do
#it for me
mySummary = function(mod){
  sum = summary(mod)
  coef = sum$coefficients[,c(1,4,5)] %>% 
    as.data.frame() %>% rownames_to_column() %>% tibble() %>%
    rename(variable = rowname,
           se = `Robust S.E.`,
           z = `Robust z`)%>%
    mutate(LCL = Estimate-qnorm(0.975)*se,
           UCL = Estimate+qnorm(0.975)*se,
           pValue=round(2*(1-pnorm(abs(z))),4)
    ) %>%
    relocate(variable,Estimate,LCL,UCL)
  coef
}

mySummary(mod01) %>%
  kable(digits=c(0,3,2,2,2,2,4)) %>% kable_styling()


## ----gee02------------------------------------------------------------

#let's look at the other models
mod02 =  gee(bdi ~ bdi.pre + treatment + length + drug, 
             data = BtheB.long.model, id = ID, family = gaussian, corstr = "exchangeable")
mod03 =  gee(bdi ~ bdi.pre + treatment + length + drug, 
             data = BtheB.long.model, id = ID, family = gaussian, corstr = "unstructured")


## ----gee.AR1----------------------------------------------------------
#this breaks because there are people with only 1 observation
#mod04 =  gee(bdi ~ bdi.pre + treatment + length + drug, data = BtheB.long.model, id = ID, family = gaussian, corstr = "AR-M")

#drop the people with a single observation
b2 = BtheB.long.model %>% drop_na() %>% group_by(ID) %>% filter(n()>1)
mod04 =  gee(bdi ~ bdi.pre + treatment + length + drug, 
             data = b2, id = ID, family = gaussian, corstr = "AR-M")

#another short custom function to avoid copy+paste too much
kableMod = function(mod,cap=''){
  mySummary(mod) %>%
    kable(digits=c(0,3,2,2,2,2,4),caption=cap) %>% kable_styling()
}

kableMod(mod02,cap='Exchangeable')
kableMod(mod03,cap='Unstructured')
kableMod(mod04,cap='AR-M*')



## ----gee03------------------------------------------------------------
#let's look at the four correlation matrices
cor01 = mod01$working.correlation
cor02 = mod02$working.correlation
cor03 = mod03$working.correlation
cor04 = mod04$working.correlation

cor01 %>% kable(digits=4,caption='Independent') %>% kable_styling()
cor02 %>% kable(digits=4,caption='Exchangable') %>% kable_styling()
cor03 %>% kable(digits=4,caption='Unstructured') %>% kable_styling()
cor04 %>% kable(digits=4,caption='AR-M') %>% kable_styling()



## ----breakout---------------------------------------------------------

data(respiratory)


