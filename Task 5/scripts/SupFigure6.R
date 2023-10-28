### task 4.5

library(readxl)
library(tidyverse)
library(reshape2)
library(cowplot)

## plot the survival probabilities of surviving to 2019 with and without extra transfusion risk

hazards_raw <- readRDS("Task 5/outputs/hazards_England.RDS") %>% 
  mutate(years_since_transfusion = survival_year - transfusion_year)

transfusion_hazards <- read_excel("Model/data/data_for_R_model.xlsx",sheet="transfusion_hazards_age")

hazards <- merge(hazards_raw,transfusion_hazards,
                 by=c("transfusion_age","sex","years_since_transfusion"),all.x=TRUE) %>%
  mutate(transfusion_survival_hazard = ifelse(is.na(transfusion_survival_hazard),1,transfusion_survival_hazard),
         qx_trans = qx*transfusion_survival_hazard,
         sx = 1 - qx,
         sx_trans = 1 - qx_trans)

survival <- hazards %>% #filter(survival_year<=year_to_survive_to) %>% 
  group_by(sex,transfusion_age,transfusion_year) %>%
  summarise(survival_prob = prod(sx),
            survival_prob_trans = prod(sx_trans)) %>%
  mutate(survival_prob = ifelse(is.na(survival_prob),0,survival_prob),
         survival_prob_trans = ifelse(is.na(survival_prob_trans),0,survival_prob_trans))


survival_melt <- melt(survival,
                      measure.vars=c("survival_prob","survival_prob_trans")) %>%
  mutate(variable_label = factor(ifelse(variable=="survival_prob","Without additional transfusion hazards",
                                 "With additional transfusion hazards"),
         levels=c("Without additional transfusion hazards",
                  "With additional transfusion hazards")))

ggplot(survival_melt,
       aes(x=transfusion_year,y=value,col=transfusion_age))+
  geom_line()+
  facet_grid(sex~variable_label)+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))+
  labs(x="Year of transfusion",y="Probability of surviving to 2019",
       col="Age group at \ntransfusion")+
  scale_colour_viridis_d(option="magma",begin=0.9,end=0)
ggsave("Task 5/outputs/SupFigure6.png",height=4,width=7)

