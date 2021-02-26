## ----setup, include=FALSE---------------------------------------------

library(Hmisc)
library(kableExtra)
library(dplyr)
library(tidyr)


knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE, fig.width=8, fig.height=5)
#This option should force knitr to use the project workng directory
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

options(full_width = FALSE)
options(knitr.kable.NA = '')



## ----colors-----------------------------------------------------------
library(RColorBrewer)

palette(c("black",brewer.pal(9,'Set1')))

dat = read.csv("data/framinghamFirst.csv",header=TRUE,
               na.strings=".",stringsAsFactors=FALSE)
dat$BMIGroups = cut(dat$BMI,breaks=c(0,18.5,25,30,Inf),
                    labels=c("Underweight","Normal","Overweight","Obese"))



## ----linePlot01-------------------------------------------------------
x=1:10
y=runif(10,0,1)
plot(x,y,pch=19)

par(mfrow=c(2,2))
plot(x,y,pch=19,type='p',col=1,main="type='p'")
plot(x,y,pch=19,type='l',col=2,main="type='l'")
plot(x,y,pch=19,type='b',col=3,main="type='b'")
plot(x,y,pch=19,type='o',col=4,main="type='o'")



## ----line.cars01------------------------------------------------------

head(mtcars)
plot(mtcars$mpg,mtcars$disp)
plot(mtcars$mpg,mtcars$disp,pch=mtcars$gear,xlab='MPG',ylab='Displacement')
plot(mtcars$mpg,mtcars$disp,col=mtcars$gear,xlab='MPG',ylab='Displacement',pch=19)
legend("topright",pc=19,col=3:5,legend=3:5)



## ----hist01-----------------------------------------------------------
hist(dat$SYSBP)
hist(dat$SYSBP,col=2,xlab='Systolic Blood Pressure',main='')
box()
#this one looks bad
hist(dat$BMI,breaks=c(0,18.5,25,30,60),freq=TRUE)


## ----hist02-----------------------------------------------------------
hist(dat$SYSBP,col=2,xlab='Systolic Blood Pressure',main='',xaxs='i',yaxs='i')
box()


## ----barplots01-------------------------------------------------------

tab.sex.smoke  = table(dat$SEX,dat$CURSMOKE)
tab.sex.smoke
#a great example of why variables should be converted to factors with meaningful labels
dat$SEX = factor(dat$SEX,levels=1:2,labels=c("M",'F'))
dat$CURSMOKE = factor(dat$CURSMOKE,levels=0:1,labels=c("Non-smoker","Smoker"))

tab.sex.smoke  = table(dat$SEX,dat$CURSMOKE)
tab.sex.smoke

barplot(tab.sex.smoke)


## ----barplots02-------------------------------------------------------

barplot(tab.sex.smoke,beside=TRUE,col=3:2)
legend("topright",fill=3:2,legend=c("M","F"))

barplot(tab.sex.smoke,beside=TRUE,col=3:2,horiz=TRUE)
legend("bottomright",fill=3:2,legend=c("M","F"))



## ----barplots03-------------------------------------------------------

x=barplot(tab.sex.smoke,beside=TRUE,col=3:2)
axis(side=1,#which side to plot on, in the order bottom, left, right, top
     at=x,#where to draw the axis labels
     labels=c("M","F","M","F"),#what to label 
     tick=FALSE,#surpress the tick marks
     line = -1)#I want it 1 line CLOSER to the figure


## ----boxplots01-------------------------------------------------------

boxplot(dat$TOTCHOL)
boxplot(TOTCHOL~SEX,data=dat,col=3:2)
#here's where we can make use of the other colour sets R colour brewer
cols = brewer.pal(4,'Paired')
#I actually want blue and red, so I'll take the 6 colours and drop the middle two
cols = brewer.pal(6,'Paired')[c(1,2,5,6)]
boxplot(TOTCHOL~CURSMOKE+SEX,data=dat,col=cols)
#and I want to shrink the y-axis to get a better sense
boxplot(TOTCHOL~CURSMOKE+SEX,data=dat,col=cols,ylim=c(100,450))



## ----breakoutSesson, echo=FALSE---------------------------------------

#first plot, basic scatterplot, labeled axes
plot(dat$SYSBP,dat$DIABP,xlab='Systolic',ylab='Diastolic')
#second plot, basic histogram of sysbp, but colored and labeled
hist(dat$SYSBP,col=2,xlab='Systolic Blood Pressure',main='')
#third plot, systolic boxplots by sex
boxplot(SYSBP~SEX,data=dat,col=3:2)
#fourth plot, barplot of BMIGroups
barplot(table(dat$BMIGroups))
#fifth plot - subgrouping
ind = which(dat$BMIGroups=='Normal')
plot(dat$SYSBP[ind],dat$DIABP[ind],xlab='SYSBP',ylab='DIABP',main='BP Correlations for people with NORMAL BMI',col=3)
#sixth plot - all subgroups
par(mfrow=c(2,2))
levs=levels(dat$BMIGroups)
limx=range(dat$SYSBP)
limy=range(dat$DIABP)
for(i in 1:length(levs)){
  ind = which(dat$BMIGroups==levs[i])
  plot(dat$SYSBP[ind],dat$DIABP[ind],xlab='SYSBP',ylab='DIABP',main=levs[i],col=i+1,xlim=limx,ylim=limy)
  #add lines of best fit if desired
#  mod = lm(DIABP~SYSBP,data=dat[ind,])
#  abline(mod,col=1,lwd=2)
}


## ----imageSaving------------------------------------------------------

pdf("savedImages.pdf",height=5,width=8)
plot(mtcars$mpg,mtcars$disp)
plot(mtcars$mpg,mtcars$disp,pch=mtcars$gear,xlab='MPG',ylab='Displacement')
plot(mtcars$mpg,mtcars$disp,col=mtcars$gear,xlab='MPG',ylab='Displacement',pch=19)
legend("topright",pc=19,col=3:5,legend=3:5)
dev.off()



## ----preview----------------------------------------------------------
#does that look like an interaction in the SEX*CURSMOKE Boxplot to anyone else? 
mod01 = lm(TOTCHOL~CURSMOKE*SEX,data=dat)
car::Anova(mod01)
#Hmmm, significant interaction in the Anova, let's dig deeper
library(margins)
pred = prediction(mod01,at=list(SEX=levels(dat$SEX),CURSMOKE=levels(dat$CURSMOKE)),calculate_se = TRUE)
summary(pred)

#so it looks like the sex difference disappears in smokers, let's visualize
#...and now it gets really weird
d = as.data.frame(summary(pred))
names(d) = gsub("at\\((.*)\\)","\\1",names(d))#ok now you're just showing off
#This is a good ggplot function learn how ggplot is built in parts
ggplot(d,aes(x=SEX, y=Prediction, colour=CURSMOKE, group=CURSMOKE))+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=0.05,size=2)+
  geom_line(size=2)+
  geom_point()
#I can't decide if the fat lines are better
ggplot(d,aes(x=CURSMOKE, y=Prediction, colour=SEX, group=SEX))+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=0.05)+
  geom_line()+
  geom_point()

#we'll end with the marginal differences - this is one of the weaker
#R libraries I have to use consistently, but it still gets the job done
marg01 = margins(mod01,at=list(SEX=levels(dat$SEX)))
summary(marg01)
marg02 = margins(mod01,at=list(CURSMOKE=levels(dat$CURSMOKE)))
summary(marg02)

