data(respiratory)

dat.resp = respiratory %>% tibble %>%
  mutate(month=paste0("month.",month))%>%
  pivot_wider(names_from=month,values_from=status)%>%
  relocate(subject)

#question 1: 
table1(~centre+sex+age+month.0+month.1+month.2+month.3+month.4|treatment,data=dat.resp)

#question 2
#a) lineplot
respiratory %>% tibble %>%
  mutate(status = 1*(status=='good')) %>%
  ggplot(aes(x=month,y=status)) +
  geom_smooth(aes(group=treatment,colour=treatment),se=FALSE)

respiratory %>% tibble %>%
  mutate(status = 1*(status=='good')) %>%
  group_by(treatment,month) %>%
  summarize(prop=mean(status))%>%
  ggplot(aes(x=month,y=prop,group=treatment,color=treatment)) +
  geom_point()+
  geom_line()

respiratory %>% tibble %>%
  mutate(status = 1*(status=='good')) %>%
  group_by(treatment,month) %>%
  summarize(good=sum(status),
            poor=sum(1-status))%>%
  pivot_longer(cols=c(good,poor))%>%
  ggplot(aes(x=interaction(name,treatment,month),y=value,fill=interaction(treatment,name)))+
  geom_bar(position='dodge',stat='identity')+
  theme(axis.text.x = element_text(angle=90))+
  scale_fill_brewer(palette='Paired')

#just converting the outcome to binary
dat.resp01 = respiratory %>% 
  mutate(status = 1*(status=='good'))

modResp01 = gee(status~centre+treatment+sex+age,data=dat.resp01,family=binomial,id=subject,corstr='AR-M')
