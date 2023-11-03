### probabilistic version of the model 

rm(list = ls())

library(tidyverse)
library(readxl)
library(mc2d)
library(reshape2)

set.seed(2+2)

n_sim <- 10000

nation = "England"
#nation = "Scotland"
units_per_donation_scenario = "NBTSS"
#units_per_donation_scenario = "S&G"

# baseline scenarios - need setting for each nation 

if(nation %in% c("England", "Wales", "Northern Ireland")){
deferral_effect_year <- 1985
calibration_prevalence_1991 <- rbeta(n = n_sim, shape1 = 532, shape2 =  808938)*100  #0.066 
} else { # Scottish baseline 
  if(nation == "Scotland"){
  deferral_effect_year <- 1984
  calibration_prevalence_1991 <- rbeta(n = n_sim, shape1 = 159, shape2 =  180000)*100  #0.088 
        }
  }


## Common across nations

deferral_effect <- rnorm(mean = 0.67, sd = 0.0765, n = n_sim) # high
deferral_effect <- 1/(1-deferral_effect)
non_idu_contribution <- 0.25

donor_non_clearance <- 1 - rnorm(n = n_sim, mean = 0.26, sd = 0.018)
survivor_non_clearance <-  1 - rnorm(n = n_sim, mean = 0.18, sd = 0.028)
trans_survival_years <- 10 ## 5 or 10 only
idu_type = "ever" 
#idu_type = "past"

contamination_constant <- calibration_prevalence_1991*non_idu_contribution

## data for long-term scenario
trans_survival_hazard_scenario <- "age"

hepc_survival_hazard <- rnorm(n = n_sim, mean = 0.425, sd = 0.137)
hepc_survival_hazard <- exp(hepc_survival_hazard)
year_to_survive_to <- 2019


source("Model/scripts/probabilistic_model_function.R")

res_list <- list()

tictoc::tic()
for (m in 1:n_sim){
  res_list[[m]] <- task4_probabilistic_four_nations(deferral_effect = deferral_effect[m],
                                            idu_type = idu_type,
                                            deferral_effect_year = deferral_effect_year,
                                            calibration_prevalence_1991 = calibration_prevalence_1991[m],
                                            donor_non_clearance = donor_non_clearance[m],
                                            contamination_constant = contamination_constant[m],
                                            trans_survival_years = trans_survival_years,
                                            survivor_non_clearance = survivor_non_clearance[m],
                                            hepc_survival_hazard = hepc_survival_hazard[m],
                                            nation = nation, 
                                            units_per_donation_scenario = units_per_donation_scenario
                                            )
}
tictoc::toc()


res_list_tab1 <- c()
res_list_tab2 <- c()
res_list_tab3 <- c()
res_list_tab4 <- c()
res_list_tab5 <- c()
res_list_tab6 <- c()
res_list_summary <- c()
res_list_survival_summary <- c()
res_list_sens_table_summary <-c()
res_list_surv_sex_summary <- c()

tictoc::tic()
tabs_1 <- lapply(res_list,"[[","table1")
tabs_2 <- lapply(res_list,"[[","table2")
tabs_3 <- lapply(res_list,"[[","table3")
tabs_4 <- lapply(res_list,"[[","table4")
tabs_5 <- lapply(res_list,"[[","table5")
tabs_6 <- lapply(res_list,"[[","table6")
tabs_summary <- lapply(res_list,"[[","summary")
tabs_survival_summary <- lapply(res_list,"[[","survival_summary")
tabs_sens_summary <- lapply(res_list, "[[", "sens_table")
tabs_surv_sex <- lapply(res_list, "[[", "survivors_sex")
tictoc::toc()


# vector of numbers to name the lists
sims <- seq(1, n_sim, 1)

tictoc::tic()
names(tabs_1) <- sims
names(tabs_2) <- sims
names(tabs_3) <- sims
names(tabs_4) <- sims
names(tabs_5) <- sims # name all the elements of the list
names(tabs_6) <- sims
names(tabs_summary) <- sims
names(tabs_survival_summary) <- sims
names(tabs_sens_summary) <- sims
names(tabs_surv_sex) <- sims
tictoc::toc()

tictoc::tic()
tabs_1_named <-  map2(tabs_1, names(tabs_1), ~ mutate(.x, rep_n = .y)) # make a column of the name
tabs_2_named <-  map2(tabs_2, names(tabs_2), ~ mutate(.x, rep_n = .y)) # make a column of the name
tabs_3_named <-  map2(tabs_3, names(tabs_3), ~ mutate(.x, rep_n = .y)) # make a column of the name
tabs_4_named <-  map2(tabs_4, names(tabs_4), ~ mutate(.x, rep_n = .y)) # make a column of the name
tabs_5_named <-  map2(tabs_5, names(tabs_5), ~ mutate(.x, rep_n = .y)) # make a column of the name
tabs_6_named <-  map2(tabs_6, names(tabs_6), ~ mutate(.x, rep_n = .y)) # make a column of the name
tabs_summary_named <-  map2(tabs_summary, names(tabs_summary), ~ mutate(.x, rep_n = .y)) # make a column of the name
tabs_summary_survival_named <-  map2(tabs_survival_summary, names(tabs_survival_summary), ~ mutate(.x, rep_n = .y)) # make a column of the name
tabs_summary_sens_named <-  map2(tabs_sens_summary, names(tabs_sens_summary), ~ mutate(.x, rep_n = .y)) # make a column of the name
tabs_summary_surv_sex_named <-  map2(tabs_surv_sex, names(tabs_surv_sex), ~ mutate(.x, rep_n = .y)) # make a column of the name
tictoc::toc()


res_list_tab1 <- as.data.frame(data.table::rbindlist(tabs_1_named))
res_list_tab2 <- as.data.frame(data.table::rbindlist(tabs_2_named))
res_list_tab3 <- as.data.frame(data.table::rbindlist(tabs_3_named))
res_list_tab4 <- as.data.frame(data.table::rbindlist(tabs_4_named))
res_list_tab5 <- as.data.frame(data.table::rbindlist(tabs_5_named))
res_list_tab6 <- as.data.frame(data.table::rbindlist(tabs_6_named))
res_list_summary <- as.data.frame(data.table::rbindlist(tabs_summary_named))
res_list_survival_summary <- as.data.frame(data.table::rbindlist(tabs_summary_survival_named))
res_list_sens_table_summary <- as.data.frame(data.table::rbindlist(tabs_summary_sens_named))
res_list_surv_sex_summary <- as.data.frame(data.table::rbindlist(tabs_summary_surv_sex_named))



# Results for summary table on page 32 
# numbers infected
signif(median(res_list_summary$total_infected),3); signif(quantile(res_list_summary$total_infected, 0.025),3); signif(quantile(res_list_summary$total_infected, 0.975),3)
signif(median(res_list_summary$total_chronic_infected),3); signif(quantile(res_list_summary$total_chronic_infected, 0.025),3); signif(quantile(res_list_summary$total_chronic_infected, 0.975),3)

# numbers chronically infected surviving 10 years
signif(median(res_list_summary$total_chronic_survived_transfusion),3); signif(quantile(res_list_summary$total_chronic_survived_transfusion, 0.025),3); signif(quantile(res_list_summary$total_chronic_survived_transfusion, 0.975),3)

# chronically infected survived to 2019 assuming extra HCV risk
signif(median(res_list_survival_summary$chronic_infected_survived_long_hepc_trans),2); signif(quantile(res_list_survival_summary$chronic_infected_survived_long_hepc_trans, 0.025), 3); signif(quantile(res_list_survival_summary$chronic_infected_survived_long_hepc_trans, 0.975),3)

# chronic infected, died by 2019 assuming extra HCV risk
signif(median(res_list_survival_summary$died_trans_hcv),3); signif(quantile(res_list_survival_summary$died_trans_hcv, 0.025),3); signif(quantile(res_list_survival_summary$died_trans_hcv, 0.975),3)

#excess hep C deaths
signif(median(res_list_survival_summary$excess_deaths_hcv),3); signif(quantile(res_list_survival_summary$excess_deaths_hcv, 0.025),3); signif(quantile(res_list_survival_summary$excess_deaths_hcv, 0.975),3)

##########

# Table 4.7 - this focuses on infected rather than chronically infected

res_list_tab3 %>% group_by(year) %>% summarise(median = signif(median(number_infected),2),
                                              lower = signif(quantile(number_infected,0.025),2),
                                              upper = signif(quantile(number_infected,0.975),2)) %>% data.frame()

res_list_tab3 %>% group_by(rep_n) %>% summarise(total = sum(number_infected)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

signif(median(res_list_summary$total_infected_70s),2); signif(quantile(res_list_summary$total_infected_70s, 0.025),2); signif(quantile(res_list_summary$total_infected_70s, 0.975),2)
signif(median(res_list_summary$total_infected_80s_90s),2); signif(quantile(res_list_summary$total_infected_80s_90s, 0.025),2); signif(quantile(res_list_summary$total_infected_80s_90s, 0.975),2)


## Table 4.8 - infected by age and sex

# Table on page 56
res_list_tab4 %>% group_by(sex,transfusion_age) %>%
  summarise(median = signif(median(proportion),2),
            lower = signif(quantile(proportion,0.025),2),
            upper = signif(quantile(proportion,0.975),2))

res_list_tab4 %>% group_by(rep_n,sex,transfusion_year) %>% summarise(total = sum(proportion)) %>%
  group_by(sex) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2))

res_list_tab4 %>% group_by(sex,transfusion_age, rep_n) %>% summarise(total = sum(number_infected)) %>%
  group_by(sex,transfusion_age) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2))


res_list_tab4 %>% group_by(rep_n,sex) %>% summarise(total = sum(number_infected)) %>%
  group_by(sex) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2))

## Table 4.10

# first column
res_list_tab3 %>% group_by(year) %>% summarise(median = signif(median(number_chronic_infected),2),
                                               lower = signif(quantile(number_chronic_infected,0.025),2),
                                               upper = signif(quantile(number_chronic_infected,0.975),2)) %>% data.frame()

res_list_tab3 %>% group_by(rep_n) %>% summarise(total = sum(number_chronic_infected)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

# ten_year survival
res_list_tab6 %>% group_by(transfusion_year,rep_n) %>%
  summarise(total = sum(number_chronic_infected_survived_transfusion)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()


res_list_tab6 %>% group_by(rep_n) %>% summarise(total = sum(number_chronic_infected_survived_transfusion)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()


##############
# table 4.11

res_list_tab6 %>% group_by(transfusion_year,rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_all)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

res_list_tab6 %>% group_by(rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_all)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

# survive to 2019 with trans risk
res_list_tab6 %>% group_by(transfusion_year,rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_trans)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

res_list_tab6 %>% group_by(rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_trans)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

# with trans and hep c risk

# survive to 2019 with trans risk
res_list_tab6 %>% group_by(transfusion_year,rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_hepc_trans)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

res_list_tab6 %>% group_by(rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_hepc_trans)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()


# summaries for the 70,s and 80-91

seventies <- seq(1970, 1979, by = 1)
eighties <- seq(1980, 1991, 1)

#survived to 10yrs
res_list_tab6 %>% group_by(rep_n) %>% filter(transfusion_year %in% seventies) %>%
  summarise(total = sum(number_chronic_infected_survived_transfusion)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

res_list_tab6 %>% group_by(rep_n) %>% filter(transfusion_year %in% eighties) %>%
  summarise(total = sum(number_chronic_infected_survived_transfusion)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()


# no risk
res_list_tab6 %>% group_by(rep_n) %>% filter(transfusion_year %in% seventies) %>%
  summarise(total = sum(number_chronic_infected_survived_long_all)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

res_list_tab6 %>% group_by(rep_n) %>% filter(transfusion_year %in% eighties) %>%
  summarise(total = sum(number_chronic_infected_survived_long_all)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

#with trans risk
res_list_tab6 %>% filter(transfusion_year %in% seventies) %>%
  group_by(rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_trans)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

res_list_tab6 %>% filter(transfusion_year %in% eighties) %>%
  group_by(rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_trans)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()


# with trans and hep c risk

res_list_tab6 %>% filter(transfusion_year %in% seventies) %>% 
  group_by(rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_hepc_trans)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()

res_list_tab6 %>% filter(transfusion_year %in% eighties) %>% 
  group_by(rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_hepc_trans)) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2)) %>% data.frame()


# Table 4.12
# chronic infected by age and sex
res_list_tab4 %>% group_by(sex,transfusion_age, rep_n) %>% summarise(total = sum(number_chronic_infected)) %>%
  group_by(sex,transfusion_age) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2))

res_list_tab4 %>% group_by(rep_n,sex) %>% summarise(total = sum(number_chronic_infected)) %>% 
  group_by(sex) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2))


# surviving to 2019
res_list_tab6 %>% group_by(sex,transfusion_age, rep_n) %>% summarise(total = sum(number_chronic_infected_survived_long_hepc_trans)) %>%
  group_by(sex,transfusion_age) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2))

res_list_tab6 %>% group_by(rep_n,sex) %>% summarise(total = sum(number_chronic_infected_survived_long_hepc_trans)) %>% 
  group_by(sex) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2))

# excess hcv deaths
res_list_tab6 %>% group_by(sex,transfusion_age, rep_n) %>% summarise(total = sum(excess_deaths_hcv)) %>%
  group_by(sex,transfusion_age) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2))

res_list_tab6 %>% group_by(rep_n,sex) %>% summarise(total = sum(excess_deaths_hcv)) %>% 
  group_by(sex) %>%
  summarise(median = signif(median(total),2),
            lower = signif(quantile(total,0.025),2),
            upper = signif(quantile(total,0.975),2))

# Table 4.13


res_list_surv_sex_summary %>% dplyr::select(survival_age, male, female) %>% 
  group_by(survival_age) %>%
  summarise(median_female = signif(median(female),2),
            lower_female = signif(quantile(female, 0.025), 2),
            upper_female = signif(quantile(female, 0.975), 2),
            median_male = signif(median(male),2),
            lower_male = signif(quantile(male, 0.025), 2),
            upper_male = signif(quantile(male, 0.975), 2))

res_list_surv_sex_summary %>% dplyr::select(survival_age, male, female,rep_n) %>% 
  group_by(rep_n) %>% summarise(total_female = sum(female), total_male = sum(male)) %>%
  summarise(median_female = signif(median(total_female),2),
            lower_female = signif(quantile(total_female, 0.025), 2),
            upper_female = signif(quantile(total_female, 0.975), 2),
            median_male = signif(median(total_male),2),
            lower_male = signif(quantile(total_male, 0.025), 2),
            upper_male = signif(quantile(total_male, 0.975), 2))



