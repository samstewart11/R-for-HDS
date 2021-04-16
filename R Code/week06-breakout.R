library(tidyverse)

dat01 = read.csv("data/bpSubjects.csv")
dat02 = read.csv("data/bpMeasures.csv",na.strings='.')

#we need to create d1 first, then create the full off of dat02 (or vice versa)
d1 = dat01 %>%
  mutate(enrollDate=as.Date(enrollDate),
         sex=toupper(sex),
         sex=factor(sex,levels=c("M","F"),labels=c("Male","Female"))
  ) %>%
  tibble()

d.all = dat02 %>%
  mutate(
    sampleDate01 = as.Date(sampleDate01),
    sampleDate02 = as.Date(sampleDate02),
    sampleDate03 = as.Date(sampleDate03),
    sampleDate04 = as.Date(sampleDate04),
    sysbp02 = as.numeric(sysbp02)
  ) %>%
  right_join(d1) %>%
  arrange(id) %>% #to make it easier to compare to dat.all
  tibble()

d.all %>%
  select(-c(sampleDate01,sampleDate02,sampleDate03,sampleDate04))%>%
  pivot_longer(cols=c("sysbp01","sysbp02","sysbp03","sysbp04"),
               names_to="index",
               values_to="BP") %>%
  mutate(
    index = gsub("sysbp","",index)
  )

date.long = d.all %>%
  select(-starts_with("sysbp"))%>%
  pivot_longer(cols=starts_with("sampleDate"),
               names_to="index",
               values_to="sampleDate")%>%
  mutate(
    index = gsub("sampleDate","",index)
  )

d.long = bp.long %>% 
  merge(date.long) %>% tibble() %>%
  filter(!is.na(BP)) %>%
  mutate(id=factor(id))

d.long %>% ggplot(aes(x=index,y=BP,color=id,group=id)) +
  geom_line(show.legend=FALSE)+
  geom_violin(aes(x=index,y=BP))

d.long %>% ggplot(aes(x=index,y=BP)) +
  geom_violin(aes(fill=index))
