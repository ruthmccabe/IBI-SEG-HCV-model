### four nations life tables for appendix 


### specific to all nations
transfusion_hazards <- read_excel("Model/data/data_for_R_model.xlsx",
                                  sheet="transfusion_hazards_age")

hepc_survival_hazard <- 1.53


### from nation specific lifetables 

england_hazards <- readRDS("Task 5/outputs/hazards_England.RDS") %>% 
  mutate(years_since_transfusion = survival_year - transfusion_year,
         country ="England")

ni_hazards <- readRDS("Task 5/outputs/hazards_Northern Ireland.RDS") %>% 
  mutate(years_since_transfusion = survival_year - transfusion_year,
         country="Northern Ireland")

scotland_hazards <- readRDS("Task 5/outputs/hazards_Scotland.RDS") %>% 
  mutate(years_since_transfusion = survival_year - transfusion_year,
         country="Scotland")

wales_hazards <- readRDS("Task 5/outputs/hazards_Wales.RDS") %>% 
  mutate(years_since_transfusion = survival_year - transfusion_year,
         country="Wales")



hazards_raw <- rbind(england_hazards,
                 ni_hazards,
                 scotland_hazards,
                 wales_hazards)

hazards <- merge(hazards_raw,transfusion_hazards,
                 by=c("transfusion_age","sex","years_since_transfusion"),all.x=TRUE) %>%
  mutate(transfusion_survival_hazard = ifelse(is.na(transfusion_survival_hazard),1,transfusion_survival_hazard),
         qx_hepc = qx*hepc_survival_hazard,
         qx_trans = qx*transfusion_survival_hazard,
         qx_hepc_trans = qx*hepc_survival_hazard*transfusion_survival_hazard,
         sx = 1 - qx,
         sx_hepc = 1 - qx_hepc,
         sx_trans = 1 - qx_trans,
         sx_hepc_trans = 1 - qx_hepc_trans)

survival <- hazards %>% #filter(survival_year<=year_to_survive_to) %>% 
  group_by(country,sex,transfusion_age,transfusion_year) %>%
  summarise(survival_prob = prod(sx),
            survival_prob_hepc = prod(sx_hepc),
            survival_prob_trans = prod(sx_trans),
            survival_prob_hepc_trans = prod(sx_hepc_trans)) %>%
  mutate(survival_prob = ifelse(is.na(survival_prob),0,survival_prob),
         survival_prob_hepc = ifelse(is.na(survival_prob_hepc),0,survival_prob_hepc),
         survival_prob_trans = ifelse(is.na(survival_prob_trans),0,survival_prob_trans),
         survival_prob_hepc_trans = ifelse(is.na(survival_prob_hepc_trans),0,survival_prob_hepc_trans))



### baseline 
survival_format_baseline <- dcast(survival %>% dplyr::select(country,sex,transfusion_age,transfusion_year,survival_prob),
                                 country+transfusion_year~sex+transfusion_age,value.var="survival_prob")

write.csv(survival_format_baseline,
          "Task 6/outputs/survival_baseline_formatted.csv",row.names=FALSE)


### transfusion risk 
survival_format_trans <- dcast(survival %>% dplyr::select(country,sex,transfusion_age,transfusion_year,survival_prob_trans),
                                  country+transfusion_year~sex+transfusion_age,value.var="survival_prob_trans")

write.csv(survival_format_trans,
          "Task 6/outputs/survival_trans_formatted.csv",row.names=FALSE)

### transfusion & HCV risk 
survival_format_hepc_trans <- dcast(survival %>% dplyr::select(country,sex,transfusion_age,transfusion_year,survival_prob_hepc_trans),
                               country+transfusion_year~sex+transfusion_age,value.var="survival_prob_hepc_trans")

write.csv(survival_format_hepc_trans,
          "Task 6/outputs/survival_trans_hepc_formatted.csv",row.names=FALSE)






