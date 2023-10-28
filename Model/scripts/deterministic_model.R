### deterministic  model 

library(tidyverse)
library(readxl)
library(reshape2)



task4_deterministic_short_nation <- function(nation = "England",
                                      deferral_reduction = 0.67,
                                      idu_type = "ever",
                                      deferral_effect_year = 1985,
                                      #calibration_prevalence_1991 = 0.066,
                                      donor_non_clearance = 0.74,
                                      contamination_constant_prop = 0.25,
                                      units_per_donation_scenario = "NBTSS",
                                      #units_per_donation = 1.32,
                                      #prop_transfused = 0.58,
                                      trans_survival_years = 10,
                                      #year_to_survive_to = 2019,
                                      hepc_survival_hazard = 1.53,
                                      trans_survival_hazard_scenario = "age",
                                      survivor_non_clearance = 0.82){
  
  
  if(!(nation %in% c("England","Scotland","Wales","Northern Ireland"))){
    stop("Nation can only be 'England', 'Scotland', 'Wales' or 'Northern Ireland'.")
  }
  
  if(nation=="Scotland"&idu_type!="ever"){
    stop("If 'Scotland' selected as nation then idu_type must be 'ever'.")
  }
  
  
  if(!(units_per_donation_scenario %in% c("NBTSS","S&G"))){
    stop("units_per_donation_scenario can only be 'NBTSS' or 'S&G'.")
  }
  
  
  if(nation!="Scotland"&units_per_donation_scenario=="S&G"){
    stop("Can only use 'S&G' for units_per_donation_scenario for 'Scotland'.")
  }

  
  if(trans_survival_years!=5&trans_survival_years!=10){
    stop("Post-transfusion survival can be '5' or '10' years only.")
  }
  
  if(contamination_constant_prop>1|contamination_constant_prop<0){
    stop("contamination_constant_prop must be between 0 and 1.")
  }
  
  if(trans_survival_hazard_scenario !="basic"&trans_survival_hazard_scenario!="age"&trans_survival_hazard_scenario!="none"){
    stop("trans_survival_hazard_scenario must equal 'basic', 'age' or 'none'.")
  }
  
  
  # Table 1: prevalence of HCV donors 
  
  calibration_prevalence_1991 <- round(read_excel("Model/data/data_for_R_model.xlsx",
                                            sheet="calibration_1991_nations") %>%
    filter(country==nation) %>% summarise(positive_tests/total_tests) %>% as.numeric()*100,3)
  contamination_constant <- contamination_constant_prop*calibration_prevalence_1991
  
  idu_calibration_point <- (calibration_prevalence_1991 - contamination_constant)*donor_non_clearance
  non_idu_constant <- contamination_constant*donor_non_clearance
  deferral_effect <- round(1/(1-deferral_reduction),2)
  
  table1 <- read_excel("Model/data/data_for_R_model.xlsx",
                       sheet="infected_IDUs_nations") %>% 
    filter(country==nation,
           injector_status==idu_type) %>%
    mutate(prop_of_1991 = no_hcv_infected_idus/last(no_hcv_infected_idus),
           prevalence_idu = ifelse(year>=deferral_effect_year,
                                   prop_of_1991*idu_calibration_point,
                                   prop_of_1991*idu_calibration_point*deferral_effect),
           prevalence = prevalence_idu + non_idu_constant) %>% data.frame()
  
  #print("Table 1")
  
  # Table 2: number of units transfusions 
  table2 <- read_excel("Model/data/data_for_R_model.xlsx",
                       sheet="number_of_donations_nations") %>% 
    filter(country == nation,
           source == units_per_donation_scenario) %>%
    mutate(units_transfused = donations*units_used_per_donation)
  
  #print("Table 2")
  
  # Table 3: number of people infected
  table3 <- merge(table1 %>% dplyr::select(year,prevalence),
                  table2 %>% dplyr::select(year,units_transfused),
                  all = TRUE) %>%
    mutate(number_infected = prevalence/100 * units_transfused,
           number_chronic_infected = number_infected * survivor_non_clearance)
  
  #print("Table 3")
  
  # Table 4: age and sex of recipients 
  age_sex_recipients <- read_excel("Model/data/data_for_R_model.xlsx",
                                   sheet="age_sex_recipients") %>%
    dplyr::select(-number)
  
  table4 <- c()
  
  for(year in 1:length(table3$year)){
    recips <- age_sex_recipients %>% mutate(number_infected = proportion*table3$number_infected[year],
                                            number_chronic_infected = proportion*table3$number_chronic_infected[year],
                                            transfusion_year = table3$year[year])
    table4 <- rbind(table4,recips)
  }
  
  #print("Table 4")
  
  # Table 5: post-transfusion survival
  post_transfusion_survival <- read_excel("Model/data/data_for_R_model.xlsx",
                                          sheet="post_transfusion_survival") %>% 
    filter(survival_years==trans_survival_years)
  
  table5 <- merge(table4,post_transfusion_survival %>% dplyr::select(transfusion_age,sex,survival),
                  by=c("transfusion_age","sex"),all=TRUE) %>%
    mutate(number_infected_survived_transfusion = number_infected*survival,
           number_chronic_infected_survived_transfusion = number_chronic_infected*survival) %>% 
    arrange(transfusion_year)
  
  #print("Table 5")
  
  # Table 6: survival until specified year
  
  
  hazards_raw <- readRDS(paste0("Task 5/outputs/hazards_",nation,".RDS")) %>% 
    mutate(years_since_transfusion = survival_year - transfusion_year)
  
  if(trans_survival_hazard_scenario=="basic"){
    transfusion_hazards <- read_excel("Model/data/data_for_R_model.xlsx",
                                      sheet="transfusion_hazards_basic")
  } else if(trans_survival_hazard_scenario=="age"){
    transfusion_hazards <- read_excel("Model/data/data_for_R_model.xlsx",
                                      sheet="transfusion_hazards_age")
  } else{ 
    transfusion_hazards <- read_excel("Model/data/data_for_R_model.xlsx",
                                      sheet="transfusion_hazards_none")
    }
  
  
  
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
  
  years_to_survive_to <- seq(from=min(table1$year)+trans_survival_years,to=2019,by=1)
  
  table6 <- c()
  #table7 <- c()
  
  
  for(year_to_survive_to in years_to_survive_to){
    
    ## survival to year in question
    survival_it <- suppressMessages(hazards %>% filter(survival_year<=year_to_survive_to) %>% 
                                      group_by(sex,transfusion_age,transfusion_year) %>%
                                      summarise(survival_prob = prod(sx),
                                                survival_prob_hepc = prod(sx_hepc),
                                                survival_prob_trans = prod(sx_trans),
                                                survival_prob_hepc_trans = prod(sx_hepc_trans)) %>%
                                      mutate(survival_prob = ifelse(is.na(survival_prob),0,survival_prob),
                                             survival_prob_hepc = ifelse(is.na(survival_prob_hepc),0,survival_prob_hepc),
                                             survival_prob_trans = ifelse(is.na(survival_prob_trans),0,survival_prob_trans),
                                             survival_prob_hepc_trans = ifelse(is.na(survival_prob_hepc_trans),0,survival_prob_hepc_trans)))
    
    ## table 6 based on survival to that year
    table6_it <- merge(table5 %>% dplyr::select(transfusion_year,transfusion_age,
                                                sex,number_infected,number_infected_survived_transfusion,
                                                number_chronic_infected,number_chronic_infected_survived_transfusion),
                       survival_it,
                       by=c("transfusion_year","transfusion_age","sex"),all.x=TRUE) %>%
      mutate(year_to_survive_to = year_to_survive_to,
             number_infected_survived_long_all = number_infected_survived_transfusion*survival_prob,
             #people who aren't chronically infected don't get the hepc hazards 
             #number_infected_survived_long_hepc = number_infected_survived_transfusion*survival_prob_hepc,
             number_infected_survived_long_trans = number_infected_survived_transfusion*survival_prob_trans,
             #number_infected_survived_long_hepc_trans = number_infected_survived_transfusion*survival_prob_hepc_trans,
             number_chronic_infected_survived_long_all = number_chronic_infected_survived_transfusion*survival_prob,
             number_chronic_infected_survived_long_hepc = number_chronic_infected_survived_transfusion*survival_prob_hepc,
             number_chronic_infected_survived_long_trans = number_chronic_infected_survived_transfusion*survival_prob_trans,
             number_chronic_infected_survived_long_hepc_trans = number_chronic_infected_survived_transfusion*survival_prob_hepc_trans,
             died_trans = number_chronic_infected - number_chronic_infected_survived_long_trans,
             died_trans_hcv = number_chronic_infected - number_chronic_infected_survived_long_hepc_trans,
             diff_died_trans_hepc = (number_chronic_infected - number_chronic_infected_survived_long_hepc_trans) - (number_chronic_infected - number_chronic_infected_survived_long_trans),
             died_post_trans_survival = died_trans + diff_died_trans_hepc - (number_chronic_infected - number_chronic_infected_survived_transfusion),
             excess_deaths_hcv = died_post_trans_survival*(1-(1/hepc_survival_hazard)))
 
    table6 <- rbind(table6,table6_it)
    
  }
  
  
  ## summary output
  output_summary <- data.frame(nation = nation,
                               deferral_reduction = deferral_reduction,
                               idu_type = idu_type,
                               deferral_effect_year = deferral_effect_year,
                               calibration_prevalence_1991 = calibration_prevalence_1991,
                               donor_non_clearance = donor_non_clearance,
                               contamination_constant_prop = contamination_constant_prop,
                               contamination_constant = contamination_constant,
                               units_per_donation_scenario = units_per_donation_scenario,
                               #units_per_donation = units_per_donation,
                               #prop_transfused = prop_transfused,
                               trans_survival_years = trans_survival_years,
                               year_to_survive_to = year_to_survive_to,
                               hepc_survival_hazard = hepc_survival_hazard,
                               trans_survival_hazard_scenario = trans_survival_hazard_scenario,
                               #transfusion_survival_hazard = transfusion_survival_hazard,
                               survivor_non_clearance = survivor_non_clearance,
                               total_infected = table3 %>% dplyr::select(number_infected) %>% 
                                 filter(!is.na(number_infected)) %>% sum() %>% round(),
                               total_infected_70s = table3 %>% filter(year<=1979,!is.na(number_infected)) %>%
                                 dplyr::select(number_infected) %>% sum() %>% round(),
                               total_infected_80s_90s = table3 %>% filter(year>1979,!is.na(number_infected)) %>%
                                 dplyr::select(number_infected) %>% sum() %>% round(),
                               total_chronic_infected = table3 %>% dplyr::select(number_chronic_infected) %>% 
                                 filter(!is.na(number_chronic_infected)) %>% sum() %>% round(),
                               total_chronic_infected_70s = table3 %>% 
                                 filter(year<=1979,!is.na(number_chronic_infected)) %>%
                                 dplyr::select(number_chronic_infected) %>% sum() %>% round(),
                               total_chronic_infected_80s_90s = table3 %>% 
                                 filter(year>1979,!is.na(number_chronic_infected)) %>%
                                 dplyr::select(number_chronic_infected) %>% sum() %>% round(),
                               total_survived_transfusion = table5 %>% 
                                 filter(!is.na(number_infected_survived_transfusion)) %>% 
                                 dplyr::select(number_infected_survived_transfusion) %>% sum() %>% round(),
                               total_survived_transfusion_70s = table5 %>%
                                 filter(transfusion_year<=1979,!is.na(number_infected_survived_transfusion)) %>%
                                 dplyr::select(number_infected_survived_transfusion ) %>% sum() %>% round(),
                               total_survived_transfusion_80s_90s = table5 %>%
                                 filter(transfusion_year>1979,!is.na(number_infected_survived_transfusion)) %>%
                                 dplyr::select(number_infected_survived_transfusion) %>% sum() %>% round(),
                               total_chronic_survived_transfusion = table5 %>% 
                                 filter(!is.na(number_chronic_infected_survived_transfusion)) %>% 
                                 dplyr::select(number_chronic_infected_survived_transfusion) %>% sum() %>% round(),
                               total_chronic_survived_transfusion_70s = table5 %>%
                                 filter(transfusion_year<=1979,
                                        !is.na(number_chronic_infected_survived_transfusion)) %>%
                                 dplyr::select(number_chronic_infected_survived_transfusion ) %>% sum() %>% round(),
                               total_chronic_survived_transfusion_80s_90s = table5 %>%
                                 filter(transfusion_year>1979,!is.na(number_chronic_infected_survived_transfusion)) %>%
                                 dplyr::select(number_chronic_infected_survived_transfusion) %>% sum() %>% round()
  )
  
  
  output_survival <- table6 %>% group_by(year_to_survive_to) %>% 
    summarise(infected_survived_long_all = sum(number_infected_survived_long_all,na.rm=TRUE),
              infected_survived_long_trans = sum(number_infected_survived_long_trans,na.rm=TRUE),
              chronic_infected_survived_long_all = sum(number_chronic_infected_survived_long_all,na.rm=TRUE),
              chronic_infected_survived_long_hepc = sum(number_chronic_infected_survived_long_hepc,na.rm=TRUE),
              chronic_infected_survived_long_trans = sum(number_chronic_infected_survived_long_trans,na.rm=TRUE),
              chronic_infected_survived_long_hepc_trans = sum(number_chronic_infected_survived_long_hepc_trans,
                                                              na.rm=TRUE),
              died_trans = sum(died_trans,na.rm = TRUE),
              died_trans_hcv = sum(died_trans_hcv,na.rm = TRUE),
              diff_died_trans_hepc = sum(diff_died_trans_hepc,na.rm = TRUE),
              died_post_trans_survival = sum(died_post_trans_survival,na.rm = TRUE),
              excess_deaths_hcv = sum(excess_deaths_hcv,na.rm=TRUE)) %>%
    mutate(total_infected = table3 %>% dplyr::select(number_infected) %>% 
             filter(!is.na(number_infected)) %>% sum() %>% round(),
           total_chronic_infected = table3 %>% dplyr::select(number_chronic_infected) %>% 
             filter(!is.na(number_chronic_infected)) %>% sum() %>% round(),
           total_survived_transfusion = table5 %>% 
             filter(!is.na(number_infected_survived_transfusion)) %>% 
             dplyr::select(number_infected_survived_transfusion) %>% sum() %>% round(),
           total_chronic_survived_transfusion = table5 %>% 
             filter(!is.na(number_chronic_infected_survived_transfusion)) %>% 
             dplyr::select(number_chronic_infected_survived_transfusion) %>% sum() %>% round(),
           # died_trans = total_chronic_infected - chronic_infected_survived_long_trans,
           # diff_died_trans_hepc = (total_chronic_infected - chronic_infected_survived_long_hepc_trans) - (total_chronic_infected - chronic_infected_survived_long_trans),
           # died_post_trans_survival = died_trans + diff_died_trans_hepc - (total_chronic_infected - total_chronic_survived_transfusion),
           # excess_deaths_hcv = died_post_trans_survival *(1-(1/hepc_survival_hazard)),
           nation = nation,
           deferral_reduction = deferral_reduction,
           idu_type = idu_type,
           deferral_effect_year = deferral_effect_year,
           calibration_prevalence_1991 = calibration_prevalence_1991,
           donor_non_clearance = donor_non_clearance,
           contamination_constant = contamination_constant,
           contamination_constant_prop = contamination_constant_prop,
           units_per_donation_scenario = units_per_donation_scenario,
           #units_per_donation = units_per_donation,
           #prop_transfused = prop_transfused,
           trans_survival_years = trans_survival_years,
           year_to_survive_to = year_to_survive_to,
           hepc_survival_hazard = hepc_survival_hazard,
           #transfusion_survival_hazard = transfusion_survival_hazard,
           survivor_non_clearance = survivor_non_clearance,
           trans_survival_hazard_scenario = trans_survival_hazard_scenario,
           survivor_non_clearance = survivor_non_clearance)
  
  ## gives the numbers from the table so hopefully don't have to think about it
  output_sens_table <- output_survival %>%
    mutate(infected_died_long_trans = total_infected - infected_survived_long_trans) %>%
    dplyr::select(nation,year_to_survive_to,deferral_reduction,idu_type,deferral_effect_year,
                  calibration_prevalence_1991,
                  donor_non_clearance,contamination_constant,contamination_constant_prop,
                  units_per_donation_scenario,trans_survival_years,
                  hepc_survival_hazard,survivor_non_clearance,trans_survival_hazard_scenario,
                  total_infected,total_chronic_infected,
                  #total_survived_transfusion,
                  total_chronic_survived_transfusion,
                  #infected_survived_long_trans,infected_died_long_trans,
                  #chronic_infected_survived_long_trans,
                  chronic_infected_survived_long_hepc_trans,
                  #died_trans,
                  died_trans_hcv,
                  excess_deaths_hcv)
  
  
 #  ## need the gender split table of survivors for convenience in sens analysis
  
 age_lookup <- hazards %>% dplyr::select(transfusion_age,transfusion_age_midpoint) %>% unique()
 survivors_age_at_survival <- suppressMessages(merge(table6 %>% dplyr::select(year_to_survive_to,
                           transfusion_year,
                           transfusion_age,sex,
                           number_chronic_infected_survived_long_hepc_trans),
               age_lookup,all.x=TRUE) %>%
   arrange(year_to_survive_to) %>%
   mutate(age_at_year_to_survive_to = transfusion_age_midpoint + (year_to_survive_to - transfusion_year),
          survival_age = ifelse(age_at_year_to_survive_to>=0&age_at_year_to_survive_to<10,"0 - 9",
                                ifelse(age_at_year_to_survive_to>=10&age_at_year_to_survive_to<20,"10 - 19",
                                       ifelse(age_at_year_to_survive_to>=20&age_at_year_to_survive_to<30,"20 - 29",
                                              ifelse(age_at_year_to_survive_to>=30&age_at_year_to_survive_to<40,
                                                     "30 - 39",
                                                     ifelse(age_at_year_to_survive_to>=40&age_at_year_to_survive_to<50,
                                                            "40 - 49",
                                                            ifelse(age_at_year_to_survive_to>=50&age_at_year_to_survive_to<60,
                                                                   "50 - 59",
                                                                   ifelse(age_at_year_to_survive_to>=60&age_at_year_to_survive_to<70,
                                                                          "60 - 69",
                                                                          ifelse(age_at_year_to_survive_to>=70&age_at_year_to_survive_to<80,
                                                                                 "70 - 79",
                                                                                 ifelse(age_at_year_to_survive_to>=80&age_at_year_to_survive_to<90,
                                                                                        "80 - 89",
                                                                                        ifelse(age_at_year_to_survive_to>=90&age_at_year_to_survive_to<100,
                                                                                               "90+",NA))))))))))) %>% group_by(year_to_survive_to,survival_age,sex) %>% 
   filter(!is.na(number_chronic_infected_survived_long_hepc_trans),
          !is.na(survival_age)) %>%
   summarise(total = sum(number_chronic_infected_survived_long_hepc_trans,na.rm=TRUE)))
 
 
 survivors_sex <- dcast(survivors_age_at_survival,year_to_survive_to+survival_age~sex,value.var = "total") %>% 
   mutate(nation = nation,
          deferral_reduction = deferral_reduction,
          idu_type = idu_type,
          deferral_effect_year = deferral_effect_year,
          calibration_prevalence_1991 = calibration_prevalence_1991,
          donor_non_clearance = donor_non_clearance,
          contamination_constant = contamination_constant,
          contamination_constant_prop = contamination_constant_prop,
          units_per_donation_scenario = units_per_donation_scenario,
          #units_per_donation = units_per_donation,
          #prop_transfused = prop_transfused,
          trans_survival_years = trans_survival_years,
          year_to_survive_to = year_to_survive_to,
          hepc_survival_hazard = hepc_survival_hazard,
          #transfusion_survival_hazard = transfusion_survival_hazard,
          survivor_non_clearance = survivor_non_clearance,
          trans_survival_hazard_scenario = trans_survival_hazard_scenario,
          survivor_non_clearance = survivor_non_clearance)


  return(list(table1 = table1,
              table2 = table2,
              table3 = table3,
              table4 = table4,
              table5 = table5,
              table6 = table6,
              #table7 = table7,
              summary = output_summary,
              survival_summary = output_survival,
              sens_table = output_sens_table,
              survivors_sex = survivors_sex))
  
}



