#simple testing of the workspace
x = 7
y = 'string'
z = runif(100,0,1)

#basic types
x = 42.42 #numeric
x2 <- 42 #integer 
string = 'dolphin' #string
string2 = "another dolphin" #string
t = TRUE
t2 = FALSE

class(x)
class(x2)

#vectors
x = c(21,2,42)
y = 1:10
z = c("first","second","third","fourth")
x[1] #21
y[3] #3
z[4] #"fourth"
z[5] #NA

#simple operators and comparisons
x = 7
y = 5
x+y
x-y
x*y
x/y
x^y
x==y
x!=y
y==5
(x!=y & y==5)
(x!=y & x==y)
(x!=y | x==y)

#matrices
x = matrix(1:10,nrow=2,byrow=TRUE)
x2 = matrix(c("a","b","c","d"),nrow=2,byrow=FALSE)
x
x2
x[1,1]
help(matrix)

#factors
prov = c('NS','NS','NB','NB','PEI','NS','PEI','NB','NB','NFLD')
prov.factor  = factor(prov)
prov
prov.factor
class(prov)
class(prov.factor)

#data frames
w = 1:4
x = c("A","B","C","D")
y = prov[c(1,4,5,2)]
z = LETTERS[21:24]
df1 = data.frame(w,x,y,z)
df2 = data.frame(first=w,second=x,third=y,fourth=z,stringsAsFactors=FALSE)
df1
df2
df1[1,]
df1[,2]
df2[1,]
df2[,2]
df1[1:2,]
df2[3:4,1:3]

#dollar-sign operator
df1$x
df2$third
df2$fifth = c(5,6,7,8)
df2

#lists
l1 = list(w,x,y,z)
l2 = list(first=w,second=x,third=y,fourth=z)
l2$fifth=df1
l1[[1]]
l2[[3]]
l2$third
l2$fifth

#reading in data
dat = read.csv("data/framinghamFirst.csv",header=TRUE,na.strings=".",stringsAsFactors=FALSE)
head(dat)

#describing data
str(dat)
psych::describe(dat)
Hmisc::describe(dat)

#numbers, characters and factors
d1 = read.csv("data/testData01.csv",header=TRUE)
str(d1)
d1$num1 = as.numeric(d1$num1)
str(d1)

d2 = read.csv("data/testData01.csv",header=TRUE,na.strings = '.')
str(d2)

dat$SEX = factor(dat$SEX,levels=c(1,2),labels=c("M","F"))
table(dat$SEX)

dat$BMICat = cut(dat$BMI,breaks=c(0,18.5,25,30,Inf))

###Breakout 01: 
dat.clean = read.csv("https://raw.githubusercontent.com/dataoptimal/posts/master/data%20cleaning%20with%20R%20and%20the%20tidyverse/telecom.csv")
