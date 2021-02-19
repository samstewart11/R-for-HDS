## ----setup ---------------------------------------------
library(readxl)
library(Hmisc)
library(kableExtra)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(gridExtra)
library(tibble)
library(reshape2)

palette(c("black",brewer.pal(9,'Set1')))

## ----dataForJoining---------------------------------------------------
#investgating the actions of merge and dplyr with repeated ids
dat01 = data.frame(
  id = 1:10,
  x1 = rep(letters[1:5],2),
  x2 = rep(LETTERS[1:2],5)
)
dat02 = data.frame(
  id = c(1:6,11,12),
  y1 = rep(1:4,2),
  y2 = 'a'
)
dat03 = data.frame(
  id = rep(1:4,2),
  z1 = -1*(1:8)
)
dat01
dat02
dat03



## ----join-------------------------------------------------------------
#inner join
merge(dat01,dat02,by = 'id')
#left join
merge(dat01,dat02,all.x = TRUE)
#right join
merge(dat01,dat02,all.y=TRUE)
#outer join
merge(dat01,dat02,all.x=TRUE,all.y=TRUE)


## ----join.leftUnique--------------------------------------------------
#non-unique data: left unique
#inner
merge(dat01,dat03)
#outer
merge(dat01,dat03,all.x=TRUE,all.y=TRUE)
#left join
merge(dat01,dat03,all.x = TRUE)
#right join
merge(dat01,dat03,all.y=TRUE)


## ----join.rightUnique-------------------------------------------------
#non-unique data: right unique
#inner
merge(dat03,dat01)
#outer
merge(dat03,dat01,all.x=TRUE,all.y=TRUE)
#left join
merge(dat03,dat01,all.x = TRUE)
#right join
merge(dat03,dat01,all.y=TRUE)


## ----join.sql---------------------------------------------------------
#testing SQL queries
library(tidyquery)
query("select * from 
      dat01 left join dat02
      using(id)")


## ----longWideData-----------------------------------------------------
#melting and molding datasets in R.  

olddata_wide <- read.table(header=TRUE, text='
 subject sex control cond1 cond2
       1   M     7.9  12.3  10.7
       2   F     6.3  10.6  11.1
       3   F     9.5  13.1  13.8
       4   M    11.5  13.4  12.9
')

# Make sure the subject column is a factor
olddata_wide$subject <- factor(olddata_wide$subject)
olddata_long <- read.table(header=TRUE, text='
 subject sex condition measurement
       1   M   control         7.9
       1   M     cond1        12.3
       1   M     cond2        10.7
       2   F   control         6.3
       2   F     cond1        10.6
       2   F     cond2        11.1
       3   F   control         9.5
       3   F     cond1        13.1
       3   F     cond2        13.8
       4   M   control        11.5
       4   M     cond1        13.4
       4   M     cond2        12.9
')
# Make sure the subject column is a factor
olddata_long$subject <- factor(olddata_long$subject)
olddata_wide
olddata_long


## ----pivot_longer-----------------------------------------------------
library(tidyr)
data_long = pivot_longer(olddata_wide,
                         control:cond2,
                         names_to="condition",
                         values_to="measurement")
#I use the multi-line functions for ease of reading, but that is unnecessary
data_long = pivot_longer(olddata_wide,control:cond2,"condition","measurement")
data_long


## ----pivot_wider------------------------------------------------------
#and from long to wide
data_wide = pivot_wider(olddata_long,
                        names_from="condition",
                        values_from='measurement')
data_wide


## ----reshape2---------------------------------------------------------

#melt makes wide data long
#if you specify one of id.vars and measure.vars, 
#it will assume everything else falls in the other
melt(olddata_wide,id.vars=c("subject","sex"))
melt(olddata_wide,measure.vars = c("control","cond1","cond2"))
#specifiy both and we can drop variables
#in this one we drop sex from the dataset
melt(olddata_wide,id.vars=c("subject"),measure.vars = c("control","cond1","cond2"))
#we can set the variable and value names, as with the pivot_ functions
melt(olddata_wide,id.vars=c("subject","sex"),
     measure.vars = c("control","cond1","cond2"),
     variable.name="condition",
     value.name='measurement')


## ----dcast------------------------------------------------------------
dcast(olddata_long,subject+sex~condition,value.var="measurement")

