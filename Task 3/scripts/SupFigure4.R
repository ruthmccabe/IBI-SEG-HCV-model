### comparison of age-sex distributions

library(readxl)
library(tidyverse)


## scotland

obs_transfusions <- read_excel("Task 5/data/survivors.xlsx",sheet="recipients_by_cohort_year")

prop_trans <- obs_transfusions %>% group_by(transfusion_year) %>%
  mutate(proportion = number/sum(number))


## england

task43 <- read_excel("model/data/data_for_R_model.xlsx",sheet="age_sex_recipients")

task43_updated <- rbind(task43,
                        data.frame("transfusion_age" = "80+",
                                   "sex" = "female",
                                   "number" = 313+58,
                                   "proportion" = NA),
                        data.frame("transfusion_age" = "80+",
                                   "sex" = "male",
                                   "number" = 160+13,
                                   "proportion" = NA)) %>%
  filter(transfusion_age!="80 - 89",
         transfusion_age!="90+") %>%
  mutate(prop_updated = number/sum(number))

prop_trans_updated <- rbind(prop_trans,
                            data.frame("transfusion_age" = "0 - 9",
                                       "sex" = "female",
                                       "transfusion_year" = 1999,
                                       "number" = 126+106,
                                       "proportion" = NA),
                            data.frame("transfusion_age" = "0 - 9",
                                       "sex" = "female",
                                       "transfusion_year" = 2004,
                                       "number" = 105+47,
                                       "proportion" = NA),
                            data.frame("transfusion_age" = "0 - 9",
                                       "sex" = "female",
                                       "transfusion_year" = 2009,
                                       "number" = 101+51,
                                       "proportion" = NA),
                            data.frame("transfusion_age" = "0 - 9",
                                       "sex" = "female",
                                       "transfusion_year" = 2014,
                                       "number" = 108+67,
                                       "proportion" = NA),
                            data.frame("transfusion_age" = "0 - 9",
                                       "sex" = "male",
                                       "transfusion_year" = 1999,
                                       "number" = 200+138,
                                       "proportion" = NA),
                            data.frame("transfusion_age" = "0 - 9",
                                       "sex" = "male",
                                       "transfusion_year" = 2004,
                                       "number" = 153+61,
                                       "proportion" = NA),
                            data.frame("transfusion_age" = "0 - 9",
                                       "sex" = "male",
                                       "transfusion_year" = 2009,
                                       "number" = 139+61,
                                       "proportion" = NA),
                            data.frame("transfusion_age" = "0 - 9",
                                       "sex" = "male",
                                       "transfusion_year" = 2014,
                                       "number" = 120+62,
                                       "proportion" = NA)) %>%
  filter(transfusion_age!="0",
         transfusion_age!="1 - 9") %>% group_by(transfusion_year) %>%
  mutate(prop_updated = number/sum(number))

comparison <- merge(prop_trans_updated %>% rename(number_scotland = number,
                                                  proportion_scotland = prop_updated),
                    task43_updated %>% dplyr::select(-proportion) %>% rename(number_england = number,
                                                                             proportion_england = prop_updated),
                    by=c("transfusion_age","sex"))




ggplot(comparison %>% filter(transfusion_year==1999),aes(x=transfusion_age))+
  geom_line(aes(y=proportion_scotland,group=transfusion_year,col="SNBTS (1999 cohort)"),lwd=1)+
  geom_line(aes(y=proportion_england,group=transfusion_year,col="Wallis et al. (1994 cohort)"),lwd=1)+
  facet_grid(sex~.)+
  theme_bw()+
  labs(x="Age at transfusion",y="Proportion of recipients",col="")+
  scale_colour_manual(values=c("limegreen","darkgreen"))+
  theme(strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle=90),
        legend.position = "bottom")
ggsave("Task 3/outputs/SupFigure4.png",width=5,height=4)






