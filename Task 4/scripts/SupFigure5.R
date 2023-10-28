## task 4.4

library(readxl)
library(tidyverse)

task44 <- read_excel("Model/data/data_for_R_model.xlsx",sheet="post_transfusion_survival") %>%
  mutate(sex=ifelse(sex=="male","Male","Female"))

ggplot(task44,aes(x=as.factor(survival_years),y=survival*100,col=transfusion_age,group=transfusion_age))+
  geom_point()+
  geom_line()+
  facet_wrap(~sex)+
  theme_bw()+
  theme(strip.background=element_rect(fill="white"))+
  labs(x="Years survived post-transfusion",y="Percentage surviving (%)",col="Age group at \ntransfusion")+
  scale_colour_viridis_d(option="magma",begin=0.9,end=0)
ggsave("Task 4/outputs/SupFigure5.png",height=4,width=3)
