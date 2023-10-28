## Supplementary Figure 7

library(tidyverse)
library(cowplot)
library(reshape2)

sensitivity_survival <- read.csv("Deterministic sensitivity analyses/outputs/sensitivity_survival.csv") %>%
  filter(nation=="England")

sensitivity_survival_ever <- melt(sensitivity_survival %>% 
                                    filter(idu_type=="ever",deferral_effect_year==1985,deferral_reduction==0.67,
                                           contamination_constant==0.0165,trans_survival_hazard_scenario=="age",
                                           hepc_survival_hazard==1.53),
                                  measure.vars=c("chronic_infected_survived_long_all",
                                                 "chronic_infected_survived_long_trans",
                                                 "chronic_infected_survived_long_hepc",
                                                 "chronic_infected_survived_long_hepc_trans")) %>%
  mutate(variable_label=factor(ifelse(variable=="chronic_infected_survived_long_all","No extra",
                                      ifelse(variable=="chronic_infected_survived_long_trans","Transfusion (age)",
                                             ifelse(variable=="chronic_infected_survived_long_hepc","HCV",
                                                    ifelse(variable=="chronic_infected_survived_long_hepc_trans","Transfusion (age) & HCV",NA)))
                                      
  ),
  levels=c("No extra","Transfusion (age)","Hcv","Transfusion (age) & HCV")))



ggplot(sensitivity_survival_ever %>% filter(variable!="chronic_infected_survived_long_hepc"))+
  geom_line(aes(x=year_to_survive_to,y=value,col=variable_label,group=variable_label),lwd=1)+
  #facet_wrap(~year_to_survive_to,ncol=10)+
  labs(x="Year",y="Number chroncially infected \nin England surviving",col="Hazard")+
  theme_bw()+
  scale_x_continuous(n.breaks=length(unique(sensitivity_survival_ever$year_to_survive_to))/2)+
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle=90),
        panel.grid.minor.x = element_blank())+
  scale_color_viridis_d(option="magma",begin=0.1,end=0.7)
ggsave("Deterministic sensitivity analyses/outputs/SupFigure7.png",height=4,width=6)



