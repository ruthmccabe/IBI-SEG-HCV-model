### Task 1 - Figure 2

library(readxl)
library(tidyverse)
library(reshape2)
library(cowplot)

task41 <- read_excel("model/data/data_for_R_model.xlsx",sheet="infected_IDUs")

donor_non_clearance <- 0.74
calibration_prevalence_1991 <- 0.066 * donor_non_clearance

deferral_effect <- (1/(1-0.67))
deferral_effect_year <- 1985
contamination_constant <- 0.25*0.066 *donor_non_clearance

task41 <- task41 %>% 
  filter(injector_status=="ever") %>%
  mutate(prop_of_1991 = no_hcv_infected_idus/last(no_hcv_infected_idus),
         prevalence_idu = prop_of_1991*calibration_prevalence_1991,
         prevalence_def = ifelse(year>=deferral_effect_year,
                                 prop_of_1991*calibration_prevalence_1991,
                                 prop_of_1991*calibration_prevalence_1991*deferral_effect)) %>% 
  data.frame()



idu_calibration_point <- (calibration_prevalence_1991 - contamination_constant)
non_idu_constant <- contamination_constant

task41 <- task41 %>%  
  mutate(prevalence_idu_contam = ifelse(year>=deferral_effect_year,
                                        prop_of_1991*idu_calibration_point,
                                        prop_of_1991*idu_calibration_point*deferral_effect),
         prevalence = prevalence_idu_contam + non_idu_constant) %>% data.frame()


plot_grid(
  ggplot(task41 %>% filter(injector_status=="ever"))+
    geom_col(aes(x=year,y=no_hcv_infected_idus,fill="HCV+ ever-IDUs"))+
    geom_line(aes(x=year,y=prop_of_1991*max(no_hcv_infected_idus),col="Proportion of 1991"),lwd=1)+
    theme_bw()+
    labs(x="Year",tag="A",col="",fill="")+
    #scale_x_continuous(breaks=seq(min(task41$year),max(task41$year),1))+
    scale_y_continuous(name="Estimated number of HCV+ \never-IDUS in England",
                       sec.axis = sec_axis(~./(max(task41$no_hcv_infected_idus)/100),name="Percentage of \n1991 (%)",
                                           breaks=c(0,20,40,60,80,100)))+
    scale_fill_manual(values="black")+
    scale_color_manual(values="red")+
    theme(#axis.text.x = element_text(angle=90),
      panel.grid.minor.x = element_blank(),
      legend.position="bottom"),
  plot_grid(ggplot(task41)+
    geom_line(aes(x=year,y=prevalence_def,#linetype="Assuming ‘step-down' \nin mid 1980s",
                  col="Assuming ‘step-down' \nin mid 1980s"),lwd=1,)+         
    geom_line(aes(x=year,y=prevalence_idu,#linetype="Same as observed anti-HCV+ \ndonations in 1991",
                  col="Pegged to observed \nanti-HCV+ donations \nin 1991"),lwd=1)+
    theme_bw()+
    scale_y_continuous(limits=c(0,0.105))+
    #scale_x_continuous(breaks=seq(min(task41$year),max(task41$year),1))+
    theme(#axis.text.x = element_text(angle=90),
      #panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical")+
    #scale_linetype_manual(values=c("dashed","solid"))+
    scale_colour_manual(values=c("black","red"))+
    labs(x="Year",y="Prevalence of HCV \ninfectious donations (%)",linetype="",col="",tag="B"),
  ggplot(task41)+
    geom_line(aes(x=year,y=prevalence_idu_contam,col="IDUs"),lwd=1)+
    geom_line(aes(x=year,y=non_idu_constant,col="Non-IDUs"),lwd=1)+
    geom_line(aes(x=year,y=prevalence,col="Total"),lwd=1)+
    theme_bw()+
    scale_colour_manual(values=c("black","skyblue","royalblue"))+
    scale_y_continuous(limits=c(0,0.105))+
    labs(x="Year",y="Prevalence of HCV \ninfectious donations (%)",col="",tag="C")+
    theme(legend.position="bottom"),nrow=1,align="hv"),
  align="h",axis="lr",nrow=2)
ggsave("Task 1/outputs/Figure2.png",height=6,width=8)
#### paint for the legend ordering



