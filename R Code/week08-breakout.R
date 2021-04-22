dat.vet = veteran %>%
  mutate(
    trt = factor(trt,levels=1:2,labels=c("standard","test"))
  )

mod.vet.km01 = survfit(Surv(time,status)~trt,data=dat.vet)

ggsurvplot(mod.vet.km01,
           ggtheme = theme_grey(),
           conf.int=FALSE,
           pValue=TRUE)

mod.vet.cox01 = coxph(Surv(time,status)~trt+celltype+age,data=dat.vet)

#For mixed effects model there's a library called `mlmRev` that has a dataset called `Exam` - it records the exam scores for 4059 students from 65 schools in Inner London.  I want you to build a model to predict their normalized exam score (`normexam`) based on the available data, see `?Exam` after you install and load the library.

dat.exam = Exam

mod.lme.exam01 = lmer(normexam~sex+standLRT+(standLRT|school),data=Exam)

dat.exam %>%
  select(standLRT,normexam,school)%>%
  drop_na()%>%
  group_by(school)%>%
  filter(n()>20)%>%
  ggplot(aes(x=standLRT,y=normexam,group=school))+
  geom_point()+
  geom_smooth(method='lm',se=FALSE)
