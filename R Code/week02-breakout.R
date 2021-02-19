#this is the answer key for the week02 breakout session

#I simulated the data, because I needed very specific issues to address
n=100
set.seed(11)
dat01 = data.frame(
  id = 1:n,
  sex = sample(c("M","F"),n,replace=TRUE),
  age = round(runif(n,18,65)),
  enrollDate = as.Date(sample(as.Date("2018-01-01"):as.Date("2018-12-31"),n),origin='1970-01-01')
)

#I'm going to pull BP measurements for follow-ups
dat02 = data.frame(
  id = dat01$id,
  sampleDate01 = dat01$enrollDate+runif(n,20,40),
  sysbp01 = round(runif(n,90,160)),
  sampleDate02 = dat01$enrollDate+runif(n,50,70),
  sysbp02 = round(runif(n,90,160)),
  sampleDate03 = dat01$enrollDate+runif(n,80,100),
  sysbp03 = round(runif(n,90,160)),
  sampleDate04 = dat01$enrollDate+runif(n,160,200),
  sysbp04 = round(runif(n,90,160))
  )

#now I need to break it for process
#step 1: convert them all to characters
dat01 = data.frame(sapply(dat01,as.character))
dat02 = data.frame(sapply(dat02,as.character))

#first invoke some missingness - NAs in dat01, . in dat02
dat01$age[sample(1:n,10,replace=FALSE)] = NA
for(i in c(2,4,6,8)){
  #more breaking: break some dates by dropping days
  ind = sample(1:n,5,replace=FALSE)
  dat02[ind,i] = gsub("-[0-9][0-9]$","",dat02[ind,i])
  #invoke some missingness
  ind = sample(1:n,5,replace=FALSE)
  dat02[ind,i] = '.'
  dat02[ind,i+1] = '.'
}

#break factor levels in sex
ind = sample(1:n,30,replace=FALSE)
dat01$sex[ind] = tolower(dat01$sex[ind])

#add some inconceivable ages
ind = sample(1:n,3,replace=FALSE)
dat01$age[ind] = as.numeric(dat01$age[ind])+100

#duplicate an ID
dat01$id[79] = dat01$id[32]

#add some bad missings in sysbp03
ind = sample(1:n,3,replace=FALSE)
dat02$sysbp02[ind] = 'no reading'

#re-organizing dat02 by first visit date
dat02 = dat02[order(dat02$sampleDate01,dat02$sampleDate02),]

#dropping some ppl that didn't finish the study
dat02 = dat02[-sample(1:n,10,replace=FALSE),]

#write.csv(dat01,file='data/bpSubjects.csv',row.names = FALSE)
#write.csv(dat02,file='data/bpMeasures.csv',row.names = FALSE)

###Answers should start here
dat01 = read.csv("data/bpSubjects.csv")
dat02 = read.csv("data/bpMeasures.csv",na.strings='.')

#making dates
dat01$enrollDate = as.Date(dat01$enrollDate)
dat02$sampleDate01 = as.Date(dat02$sampleDate01)
dat02$sampleDate02 = as.Date(dat02$sampleDate02)
dat02$sampleDate03 = as.Date(dat02$sampleDate03)
dat02$sampleDate04 = as.Date(dat02$sampleDate04)

#fixing BP
dat02$sysbp02 = as.numeric(dat02$sysbp02)

#fixing sex
x01 = factor(dat01$sex,
                   levels=c("M","F","m","f"),
                   labels=c("Male","Female","Male","Female"))
dat01$sex = factor(toupper(dat01$sex),
             levels=c("M","F"),
             labels=c("Male","Female"))
Hmisc::describe(dat01)
Hmisc::describe(dat02)

#some wonky ages, but we'll ignore for now

#using merge - I think left join is the best
dat.all = merge(dat01, dat02, all.x = TRUE, all.y = FALSE)

library(reshape2)
dat.bp = melt(dat.all,
              id.vars=c("id","sex","age"),
              measure.vars=c("sysbp01","sysbp02","sysbp03","sysbp04"),
              variable.name='index',
              value.name ='BP')

#ind = order(dat.bp$id,dat.bp$index)
#dat.bp = dat.bp[ind,]

dat.dates = melt(dat.all,
                 id.vars=c("id","sex","age"),
                 measure.vars=sprintf("sampleDate%02d",1:4),
                 variable.name='index',
                 value.name='date')

#making the index variables match
dat.bp$index = substr(dat.bp$index,6,7)
dat.dates$index = gsub("[a-zA-z]+","",dat.dates$index)
#we can just stick them together
dat.long = cbind(dat.bp,date=dat.dates$date)
dat.long = dat.long[order(dat.long$id,dat.long$index),]

#merge would work as well
dat.long = merge(dat.dates,dat.bp,all=TRUE)

