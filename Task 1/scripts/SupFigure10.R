#### ever-IDUs in england vs scotland

library(tidyverse)
library(readxl)
library(cowplot)

idus_raw <- read_excel("model/data/data_for_R_model.xlsx",
                   sheet="infected_IDUs_nations") %>% filter(country=="England"|country=="Scotland",
                                                             injector_status=="ever") %>% group_by(country) %>%
  mutate(prop_1991 = no_hcv_infected_idus/last(no_hcv_infected_idus))


population <- rbind(data.frame("country" = "England",
                               "year" = unique(idus_raw$year),
                               "pop" = c(46411700,46411700,46571900,46686200,46682700,46674400,46659900,46639800,
                                         46638200,46698100,46787200,46820800,46777300,46813700,46912400,47057400,
                                         47187600,47300400,47412300,47552700,47699100,47875000)),
                    data.frame("country" = "Scotland",
                               "year" = unique(idus_raw$year),
                               "pop" = c(5235600,5235600,5230600,5233900,5240800,5232400,5233400,5226200,5212300,
                                         5203600,5193900,5180200,5164500,5148100,5138900,5127900,5111800,5099000,
                                         5077400,5078200,5081300,5083300)))


idus <- merge(idus_raw,population,by=c("country","year")) %>%
  mutate(per_100000 = no_hcv_infected_idus/pop *100000)


plot_grid(plot_grid(ggplot(idus,aes(x=year,y=no_hcv_infected_idus,fill=country))+
  geom_col()+
    facet_wrap(~country,nrow=1)+
  theme_bw()+
    scale_fill_manual(values=c("red","royalblue"))+
  labs(x="Year",y="Number of \nHCV-infectious \never-IDUs",fill="",tag="A")+
    theme(legend.position="none",
          strip.background = element_rect(fill="white")),
  ggplot(idus,aes(x=year,y=per_100000,fill=country))+
    geom_col()+
    facet_wrap(~country,nrow=1)+
    theme_bw()+
    scale_fill_manual(values=c("red","royalblue"))+
    labs(x="Year",y="Number of \nHCV-infectious \never-IDUs per \n100,000 population",fill="",tag="B")+
    theme(legend.position="none",
          strip.background = element_rect(fill="white")),
  nrow=2,align="hv"),
  ggplot(idus,aes(x=year,y=prop_1991,col=country))+
  geom_line()+
  theme_bw()+
    scale_colour_manual(values=c("red","royalblue"))+
  labs(x="Year",y="Trend in \nHCV-infectious \nIDUs relative \nto 1991 value",col="",tag="C")+
    theme(legend.position="bottom"),
  nrow=2,rel_heights = c(1.5,1),align="h",axis="tl"
  )
ggsave("Task 1/outputs/SupFigure10.png",width=5,height=6)


