## ----setup, include=FALSE------------------------------------------------------------------

knitr::opts_chunk$set(tidy = TRUE, echo = TRUE, message=FALSE, warning=FALSE, fig.width=8, fig.height=5)

#This option should force knitr to use the project workng directory
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

options(full_width = FALSE)
options(knitr.kable.NA = '')



## ----readFram------------------------------------------------------------------------------
library(Hmisc)
library(kableExtra)
library(tidyverse)
library(ggplot2)

dat = read.csv("data/framinghamFirst.csv",header=TRUE, 
               na.strings=".",stringsAsFactors=FALSE)
dat$BMIGroups = cut(dat$BMI,breaks=c(0,18.5,25,30,Inf),
                    labels=c("Underweight","Normal","Overweight","Obese"))
dat$SEX = factor(dat$SEX,levels=1:2,labels=c("Male","Female"))
dat$DIABETES = factor(dat$DIABETES,levels=0:1,labels=c("No Diabetes","Diabetes"))
dat$HYPERTEN = factor(dat$HYPERTEN,levels=0:1,labels=c("Normotensive","Hypertensive"))



## ----pipe01--------------------------------------------------------------------------------

table(dat$SEX,dat$HYPERTEN)
dat$SEX %>% table(dat$HYPERTEN)



## ----tibble01------------------------------------------------------------------------------
tibble(dat)
dat = tibble(dat)


## ----filter01------------------------------------------------------------------------------
#the full dataset
dat
#just the men
filter(dat,SEX=='Male')
#just the women, but using a pipe
dat %>% filter(SEX=='Female')
#Just the obese men
dat %>%
  filter(SEX=='Male',
         BMIGroups=='Obese')
#Just the overweight or obese people
dat %>%
  filter(BMIGroups=="Obese" | BMIGroups=="Overweight")


## ----select01------------------------------------------------------------------------------

#pick three variables
select(dat,SEX,BMIGroups,CURSMOKE)


## ----mutate01------------------------------------------------------------------------------


dat %>% 
  select(SEX,AGE,BMI,DEATH)%>%#not necessary, for demo purposes
  mutate(DEATH=factor(DEATH,levels=0:1,labels=c("Dead","Alive")),
         BMIGroups = cut(BMI,breaks=c(0,18.5,25,30,Inf)),
         BMINormal = (BMIGroups=='(18.5,25]')
         )



## ----summarise01---------------------------------------------------------------------------

dat %>%
  summarise(averageAge = mean(AGE,na.rm=TRUE))

dat %>%
  filter(!is.na(BMI))%>%
  mutate(CURSMOKE=factor(CURSMOKE,levels=0:1,labels=c("Non-smoker","Smoker")))%>%
  group_by(SEX,CURSMOKE)%>%
  summarise(
    n = n(),
    mean = mean(BMI),
    sd = sd(BMI),
    median = median(BMI)
    ) %>% 
  kable() %>% kable_styling() %>%
  row_spec(0,background='cyan',color='magenta')


## ----summarise02---------------------------------------------------------------------------
dat %>%
  filter(!is.na(BMI))%>%
  summarise(
    n = n(),
    mean = mean(BMI,na.rm=TRUE),
    sd = sd(BMI,na.rm=TRUE)
    )


## ----summarise03---------------------------------------------------------------------------
dat %>%
  filter(!is.na(BMI))%>%
  group_by(SEX)%>%
  summarise(
    n = n(),
    mean = mean(BMI,na.rm=TRUE),
    sd = sd(BMI,na.rm=TRUE)
    ) 

dat %>%
  filter(!is.na(BMI))%>%
  group_by(SEX,CURSMOKE)%>%
  summarise(
    n = n(),
    mean = mean(BMI,na.rm=TRUE),
    sd = sd(BMI,na.rm=TRUE)
    ) 


## ----summarise04---------------------------------------------------------------------------
dat %>%
  filter(!is.na(BMI))%>%
  mutate(CURSMOKE=factor(CURSMOKE,levels=0:1,labels=c("Non-smoker","Smoker")))%>%
  group_by(SEX,CURSMOKE)%>%
  summarise(
    n = n(),
    mean = mean(BMI,na.rm=TRUE),
    sd = sd(BMI,na.rm=TRUE)
    ) 


## ----kable01-------------------------------------------------------------------------------
dat %>%
  filter(!is.na(BMI))%>%
  mutate(CURSMOKE=factor(CURSMOKE,levels=0:1,labels=c("Non-smoker","Smoker")))%>%
  group_by(SEX,CURSMOKE)%>%
  summarise(
    n = n(),
    mean = mean(BMI,na.rm=TRUE),
    sd = sd(BMI,na.rm=TRUE)
    ) %>%
  kable(digits=2) %>% kable_styling()%>%
  kable_paper("striped",full_width=FALSE)%>%
  row_spec(0,bold=T,background='black',color='white')



## ----originalCode--------------------------------------------------------------------------
dat01 = read.csv("data/bpSubjects.csv")
dat02 = read.csv("data/bpMeasures.csv",na.strings='.')

####fixing dat01
#dates
dat01$enrollDate = as.Date(dat01$enrollDate)
#sex
dat01$sex = factor(toupper(dat01$sex),
             levels=c("M","F"),
             labels=c("Male","Female"))

###fixing dat02
#dates
dat02$sampleDate01 = as.Date(dat02$sampleDate01)
dat02$sampleDate02 = as.Date(dat02$sampleDate02)
dat02$sampleDate03 = as.Date(dat02$sampleDate03)
dat02$sampleDate04 = as.Date(dat02$sampleDate04)
#fixing BP
dat02$sysbp02 = as.numeric(dat02$sysbp02)

#I'll use left_join
dat.all = left_join(dat01, dat02, all.x = TRUE, all.y = FALSE)

library(reshape2)
dat.bp = melt(dat.all,
              id.vars=c("id","sex","age"),
              measure.vars=c("sysbp01","sysbp02","sysbp03","sysbp04"),
              variable.name='index',
              value.name ='BP')

dat.dates = melt(dat.all,
                 id.vars=c("id","sex","age"),
                 measure.vars=sprintf("sampleDate%02d",1:4),
                 variable.name='index',
                 value.name='date')

#once each long dataset is completed we can merge them
#merge would work as well
dat.long = merge(dat.dates,dat.bp,all=TRUE)


## ----pipedSolution-------------------------------------------------------------------------
bp.long = d.all %>% 
  select(-starts_with("sampleDate"))%>%
  pivot_longer(cols=starts_with("sysbp"))%>%
  rename(BP=value,index=name)%>%
  mutate(index=gsub("sysbp","",index))

date.long = d.all %>% 
  select(-starts_with("sysbp"))%>%
  pivot_longer(cols=starts_with("sampleDate"))%>%
  rename(sampleDate=value,index=name)%>%
  mutate(index=gsub("sampleDate","",index))

dat.long = bp.long %>% 
  merge(date.long) %>%
  filter(!is.na(BP))

