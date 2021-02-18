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

#inner join
merge(dat01,dat02,by = 'id')
#left join
merge(dat01,dat02,all.x = TRUE)
#right join
merge(dat01,dat02,all.y=TRUE)
#outer join
merge(dat01,dat02,all.x=TRUE,all.y=TRUE)

#non-unique data: left unique
#inner
merge(dat01,dat03)
#outer
merge(dat01,dat03,all.x=TRUE,all.y=TRUE)
#left join
merge(dat01,dat03,all.x = TRUE)
#right join
merge(dat01,dat03,all.y=TRUE)

#non-unique data: right unique
#inner
merge(dat03,dat01)
#outer
merge(dat03,dat01,all.x=TRUE,all.y=TRUE)
#left join
merge(dat03,dat01,all.x = TRUE)
#right join
merge(dat03,dat01,all.y=TRUE)

#testing SQL queries
library(tidyquery)
query("select * from 
      dat01 left join dat02
      using(id)")

#melting and molding datasets in R.  The website below is pretty good,
#though the function they suggest, gather, has been replaced
#http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

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
library(tidyr)
data_long = gather(olddata_wide,
                   key=condition,
                   value=measurement,
                   control:cond2,
                   factor_key=TRUE)
data_long = gather(olddata_wide,condition,measurement,control, cond1, cond2,factor_key=TRUE)
#tidyr and the other tidyverse functions can take columns names without 
#quotes, which is good, but counter-intuitive to old-timey R programmers
#gather_ (note the _) is the same function, but works how I would expect
data_long = gather_(olddata_wide,
                    key_col="condition",
                    value_col="measurement",
                    gather_cols=c("control", "cond1", "cond2"),
                    factor_key=TRUE)
#as the help file for gather explains, pivot_longer is the new command
#that they prefer you to use
data_long = pivot_longer(olddata_wide,
                         control:cond2,
                         names_to="condition",
                         values_to="measurement")
data_long = pivot_longer(olddata_wide,control:cond2,"condition","measurement")

#and from long to wide
data_wide = spread(olddata_long,condition,measurement)
data_wide = spread_(olddata_long,key="condition",value="measurement")

#historically, reshape2 was more popular than tidyr
#melt makes wide data long
library(reshape2)
#if you specify one of id.vars and measure.vars, it will assume everything
#else falls in the other
melt(olddata_wide,id.vars=c("subject","sex"))
melt(olddata_wide,measure.vars = c("control","cond1","cond2"))
#in this one we drop sex from the dataset
melt(olddata_wide,id.vars=c("subject"),measure.vars = c("control","cond1","cond2"))
melt(olddata_wide,id.vars=c("subject","sex"),
     measure.vars = c("control","cond1","cond2"),
     variable.name="condition",
     value.name='measurement')

#dcast turns long data into wide - we'll learn more about formulas later
dcast(olddata_long,subject+sex~condition,value.var="measurement")
