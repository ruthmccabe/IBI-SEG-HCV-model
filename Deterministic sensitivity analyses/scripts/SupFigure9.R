### Supplementary Figure 9

## calibration point and donor non clearance stay as is throughout

calibration_prevalence_1991 <- 0.066
donor_non_clearance <- 0.74


## contamination constants have two options 

contamination_constant <- 0.25*0.066
contamination_constant_sens <- 0*0.066

## two deferral effects 

deferral_effect <- round(1/(1-0.67),2)
deferral_effect_sens <- round(1/(1-0.33),2)

## two deferral effect years
deferral_effect_year <- 1985
deferral_effect_year_sens <- 1986


### two calibration points for the two contamination_constants 
idu_calibration_point <- (calibration_prevalence_1991 - contamination_constant)*donor_non_clearance
non_idu_constant <- contamination_constant*donor_non_clearance

idu_calibration_point_sens <- (calibration_prevalence_1991 - contamination_constant_sens)*donor_non_clearance
non_idu_constant_sens <- contamination_constant_sens*donor_non_clearance



task41_sens <- read_excel("Model/data/data_for_R_model.xlsx",sheet="infected_IDUs") %>% 
  filter(injector_status=="ever") %>% data.frame() %>%
  mutate(prop_of_1991 = no_hcv_infected_idus/last(no_hcv_infected_idus),
         ## baseline
         prevalence_idu_baseline = ifelse(year>=deferral_effect_year,
                                          prop_of_1991*idu_calibration_point,
                                          prop_of_1991*idu_calibration_point*deferral_effect),
         prevalence_baseline = prevalence_idu_baseline + non_idu_constant,
         ## change in deferral reduction 
         prevalence_idu_def_effect = ifelse(year>=deferral_effect_year,
                                            prop_of_1991*idu_calibration_point,
                                            prop_of_1991*idu_calibration_point*deferral_effect_sens),
         prevalence_def_effect = prevalence_idu_def_effect + non_idu_constant,
         ## change in deferral year
         prevalence_idu_def_year = ifelse(year>=deferral_effect_year_sens,
                                          prop_of_1991*idu_calibration_point,
                                          prop_of_1991*idu_calibration_point*deferral_effect),
         prevalence_def_year = prevalence_idu_def_year + non_idu_constant,
         ## change in contamination constant 
         prevalence_idu_cc = ifelse(year>=deferral_effect_year,
                                    prop_of_1991*idu_calibration_point_sens,
                                    prop_of_1991*idu_calibration_point_sens*deferral_effect),
         prevalence_cc = prevalence_idu_cc + non_idu_constant_sens,
  )


### filter to just have the prevalences you need so is less clunky

task41_sens_plot <- task41_sens %>% dplyr::select(year,prevalence_baseline,
                                                  prevalence_def_effect,prevalence_def_year,prevalence_cc)



plot_grid(ggplot(task41_sens_plot)+
            geom_line(aes(x=year,y=prevalence_def_effect,col="33%"),lwd=1)+
            geom_line(aes(x=year,y=prevalence_baseline,col="67%"),lwd=1)+
            theme_bw()+
            labs(x="Year",y="Prevalence infectious \ndonations (%)",col="Reduction from \ndeferral policy",tag="A")+
            theme(legend.position="bottom",axis.text.x = element_text(angle=90))+
            scale_y_continuous(limits=c(0,0.105))+
            guides(colour=guide_legend(title.position="top",
                                       title.hjust = 0.5))+
            scale_colour_viridis_d(option="plasma",begin=0.8,end=0),
          ggplot(task41_sens_plot)+
            geom_line(aes(x=year,y=prevalence_def_year,col="1986"),lwd=1)+
            geom_line(aes(x=year,y=prevalence_baseline,col="1985"),lwd=1)+
            theme_bw()+
            guides(colour=guide_legend(title.position="top",
                                       title.hjust = 0.5))+
            scale_y_continuous(limits=c(0,0.105))+
            labs(x="Year",y="Prevalence infectious \ndonations (%)",col="Year of \ndeferral policy",tag="B")+
            theme(legend.position="bottom",axis.text.x = element_text(angle=90))+
            scale_colour_viridis_d(option="plasma",begin=0,end=0.4),
          ggplot(task41_sens_plot)+
            geom_line(aes(x=year,y=prevalence_cc,col="0%"),lwd=1)+
            geom_line(aes(x=year,y=prevalence_baseline,col="25%"),lwd=1)+
            theme_bw()+
            scale_y_continuous(limits=c(0,0.105))+
            scale_colour_viridis_d(option="plasma",begin=0.6,end=0)+
            labs(x="Year",y="Prevalence infectious \ndonations (%)",col="Non-IDU \nproportion",tag="C")+
            guides(colour=guide_legend(title.position="top",
                                       title.hjust = 0.5))+
            theme(legend.position = "bottom",axis.text.x = element_text(angle=90)),ncol=3)
ggsave("Deterministic sensitivity analyses/outputs/SupFigure9.png",width=8,height=3)

