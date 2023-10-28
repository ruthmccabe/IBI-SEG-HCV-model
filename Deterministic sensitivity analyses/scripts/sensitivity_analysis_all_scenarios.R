### sensitivity analyses for the four nations

### combinations

sens_params_ever_england_ni_wales <- expand.grid(nation=c("England","Northern Ireland","Wales"),
                                           contamination_constant_prop = c(0,0.25,0.5,1),
                                           deferral_effect_year = c(1985,1986),
                                           deferral_reduction = c(0.67,0.33),
                                           idu_type = "ever",
                                           trans_survival_hazard_scenario = c("basic",
                                                                              "age","none"),
                                           hepc_survival_hazard = c(1.53,1),
                                           units_per_donation_scenario = "NBTSS")

sens_params_past_england_ni_wales <- expand.grid(nation=c("England","Northern Ireland","Wales"),
                                           contamination_constant_prop = c(0,0.25,0.5,1),
                                           deferral_effect_year = c(1986,1987),
                                           deferral_reduction = c(0.33,0.20),
                                           idu_type = "past",
                                           trans_survival_hazard_scenario = c("basic",
                                                                              "age","none"),
                                           hepc_survival_hazard = c(1.53,1),
                                           units_per_donation_scenario = "NBTSS")


sens_params_ever_scotland <- expand.grid(nation="Scotland",
                                         contamination_constant_prop = c(0,0.25,0.5,1),
                                         deferral_effect_year = c(1984,1985),
                                         deferral_reduction = c(0.67,0.33),
                                         idu_type="ever",
                                         trans_survival_hazard_scenario = c("basic",
                                                                            "age","none"),
                                         hepc_survival_hazard = c(1.53,1),
                                         units_per_donation_scenario = c("NBTSS","S&G"))


sens_params <- rbind(sens_params_ever_england_ni_wales,
                     sens_params_past_england_ni_wales,
                     sens_params_ever_scotland#,
                     #sens_params_ever_wales
                     ) %>% arrange(nation)


source("Model/scripts/deterministic_model.R")


sensitivity_summary <- c()
sensitivity_survival <- c()
sensitivity_table <- c()
sensitivity_sex <- c()


for(i in 1:nrow(sens_params)){
  
  model_it <- task4_deterministic_short_nation(nation = sens_params$nation[i],
                                               contamination_constant_prop = sens_params$contamination_constant_prop[i],
                                               deferral_effect_year = sens_params$deferral_effect_year[i],
                                               deferral_reduction = sens_params$deferral_reduction[i],
                                               idu_type = sens_params$idu_type[i],
                                        trans_survival_hazard_scenario = sens_params$trans_survival_hazard_scenario[i],
                                        hepc_survival_hazard = sens_params$hepc_survival_hazard[i],
                                        units_per_donation_scenario = sens_params$units_per_donation_scenario[i])
                                        #calibration_prevalence_1991 = sens_params$calibration_prevalence_1991[i])
  
  sensitivity_summary <- rbind(sensitivity_summary,
                               model_it$summary)
  
  sensitivity_survival <- rbind(sensitivity_survival,
                                model_it$survival_summary)
  
  sensitivity_table <- rbind(sensitivity_table,
                             model_it$sens_table)
  
  sensitivity_sex <- rbind(sensitivity_sex,
                           model_it$survivors_sex)
  
  
  print(i)
}

write.csv(sensitivity_summary,
          "Deterministic sensitivity analyses/outputs/sensitivity_summary.csv",
          row.names=FALSE)
write.csv(sensitivity_survival,
          "Deterministic sensitivity analyses/outputs/sensitivity_survival.csv",
          row.names=FALSE)
write.csv(sensitivity_table,
          "Deterministic sensitivity analyses/outputs/sensitivity_table.csv",
          row.names=FALSE)
write.csv(sensitivity_sex,
          "Deterministic sensitivity analyses/outputs/sensitivity_sex.csv",
          row.names=FALSE)


### output for ease of reading


write.csv(sensitivity_table %>% filter(year_to_survive_to==2019) %>% arrange(nation) %>%
            mutate(#infected_survived_long_trans=round(infected_survived_long_trans),
                   #infected_died_long_trans=round(infected_died_long_trans),
                   chronic_infected_survived_long_hepc_trans = round(chronic_infected_survived_long_hepc_trans),
                   died_trans_hcv = round(died_trans_hcv),
                   excess_deaths_hcv = round(excess_deaths_hcv)),
          "Deterministic sensitivity analyses/outputs/sensitivity_table_2019.csv",
          row.names=FALSE)







