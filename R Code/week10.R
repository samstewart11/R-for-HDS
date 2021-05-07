## ----setup, include=FALSE----------------------------------------------

knitr::opts_chunk$set(tidy = TRUE, echo = TRUE, message=FALSE, warning=FALSE, fig.width=8, fig.height=5)

#This option should force knitr to use the project workng directory
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

options(full_width = FALSE)
options(knitr.kable.NA = '')



## ----readData----------------------------------------------------------
library(Hmisc)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(boomer)
library(XML)
library(car)
library(rpart)
library(rattle)
library(pROC)

#lot's of ML algorithms have randomness, so it's good practice to 
#set the seed at the beginning of your work so it will produce consistent
#results
set.seed(11)

dat = read.csv("data/brfss2013-Connecticut.csv")

#variables under study
vars = c('bphigh4','cvdinfr4','cvdcrhd4','cvdstrk3','asthma3','chcscncr','chcocncr',
         'chccopd1','havarth3','addepev2','chckidny','diabete3','drvisits')

dat %>% tibble %>%
  select(vars)

#dat02 is a new version of the dataset
dat02 = data.frame(ID=rownames(dat))[,-1]

#car::recode is useful for collapsing levels in a factor
#we need the car:: part because dplyr also has a recode function, but it's mor verbose
dat02$bphigh = car::recode(dat$bphigh4, recodes="1=1;2:4=0;else=NA")
dat02$ha = car::recode(dat$cvdinfr4,recodes="1=1;2=0;else=NA")
dat02$chd = car::recode(dat$cvdcrhd4,recodes="1=1;2=0;else=NA")
dat02$stroke = car::recode(dat$cvdstrk3,recodes="1=1;2=0;else=NA")
dat02$asthma = car::recode(dat$asthma3,recodes="1=1;2=0;else=NA")
dat02$skinCa = car::recode(dat$chcscncr,recodes="1=1;2=0;else=NA")
dat02$otherCa = car::recode(dat$chcocncr,recodes="1=1;2=0;else=NA")
dat02$copd = car::recode(dat$chccopd1,recodes="1=1;2=0;else=NA")
dat02$arthritis = car::recode(dat$havarth3,recodes="1=1;2=0;else=NA")
dat02$depression = car::recode(dat$addepev2,recodes="1=1;2=0;else=NA")
dat02$kidney = car::recode(dat$chckidny,recodes="1=1;2=0;else=NA")
dat02$diabetes = car::recode(dat$diabete3,recodes="1=1;2:4=0;else=NA")
#drvisits is continuous so it doesn't need to be recoded
dat02$drvisits = dat$drvisits
dat02$drvisits[which(dat02$drvisits==88)] = 0
dat02$drvisits[which(dat02$drvisits>76)] = NA

#converting to factors in a new dataset
dat03 = do.call(cbind.data.frame,lapply(dat02,factor,levels=1:0,labels=c("Yes","No")))
dat03$drvisits = dat02$drvisits

#dropping missing values from each dataset
dat03 = na.omit(dat03)
dat02 = na.omit(dat02)

#dat02 is the numeric version of the dataset, 
#dat03 is the factor version



## ----dataSplit---------------------------------------------------------

#we'll focus on the factored data
n=dim(dat03)[1]
nSplit = round(n*0.7,0)
train = dat03[1:nSplit,]
test = dat03[(nSplit+1):n,]



## ----rpart01-----------------------------------------------------------

###a simple classification tree
mod.cart01 = rpart(bphigh~.,data=train)
mod.cart01
summary(mod.cart01)



## ----rpart02-----------------------------------------------------------
#trying to build as large a tree as possible
mod.cart02 = rpart(bphigh~.,data=train,control=list(cp=0.0,minbucket=0,minsplit=0))
mod.cart03 = rpart(bphigh~.,data=train,control=list(cp=0.0041,minbucket=1,minsplit=2))


## ----rpart.optimal-----------------------------------------------------
#getting optimal CP values
printcp(mod.cart02)


## ----plotcp------------------------------------------------------------
plotcp(mod.cart02,las=2)


## ----pruning-----------------------------------------------------------
#pruning trees is done using prune
mod.cart02a = prune(mod.cart02,cp=0.0005)
mod.cart02b = prune(mod.cart02,cp=0.004)#this should be the same mod.cart03

mod.cart01 = mod.cart02b


## ----rpart.plot--------------------------------------------------------
#plotting the trees is best through the rattle library
plot(mod.cart01)#boo :(
text(mod.cart01)
fancyRpartPlot(mod.cart01)#yay! :)


## ----evaluation--------------------------------------------------------
#predicting new values from the model
#default is probabilities
pred.cart01 = predict(mod.cart01,newdata=test)
head(pred.cart01,10)
pred.cart01 = pred.cart01[,1]#Just keep the Y probabilities


## ----roc01,fig.height=5,fig.width=5------------------------------------

roc.cart01 = roc(test$bphigh,pred.cart01,ci=TRUE,levels=c("Yes","No"))
roc.cart01
roc.dat01 = coords(roc.cart01)
roc.dat01
plot(roc.cart01)


## ----roc02-------------------------------------------------------------

opt.cart01 = coords(roc.cart01,x='best',transpose=TRUE)
pred.cart01.value = pred.cart01 >= opt.cart01[1]



## ----getAccuracyTable--------------------------------------------------
#a function to get the accuracy of the model from the predicted labels, should be of the form
#           pred=Y  pred=N
#label  Yes      a       b
#        No      c       d
getAccuracy = function(tab){
    if(any(c(colnames(tab)[1],rownames(tab)[1])%in%c("No",0,FALSE)))
        warning("Are you sure that your table is set up the right way?")
    TP = tab[1,1]
    FP = tab[2,1]
    FN = tab[1,2]
    TN = tab[2,2]
    P = rowSums(tab)[1]
    N = rowSums(tab)[2]
    acc=(TP+TN)/(P+N)
    errRate=(FP+FN)/(P+N)
    sens=TP/P
    spec=TN/N
    prec=TP/(TP+FP)
    F1=2*prec*sens/(prec+sens)
    F2=(1+2^2)*prec*sens/(2^2*prec+sens)
    F0.5=(1+0.5^2)*prec*sens/(0.5^2*prec+sens)
    out=c(acc,errRate,sens,spec,prec,F1,F2,F0.5)
    names(out) = c('acc','errRate','sens','spec','prec','F1','F2','F0.5')
    out
}

#Note that I have to flip the columns to make the diagonal line up right
tab.cart01 = table(test$bphigh,pred.cart01.value)[,2:1]
tab.cart01
getAccuracy(tab.cart01)


## ----fram01------------------------------------------------------------

fram = read.csv("data/framinghamFirst.csv",header=TRUE, 
               na.strings=".",stringsAsFactors=FALSE)
fram = fram %>%
  select(-CIGPDAY) #we obviously need to drop this

#any data formatting you should do here first
set.seed(42)
n=dim(fram)[1]
nSplit = round(n*0.7,0)
train.fram = fram[1:nSplit,]
test.fram = fram[(nSplit+1):n,]



