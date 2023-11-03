

task4_probabilistic_four_nations <- function(nation = nation,
                                             units_per_donation_scenario = units_per_donation_scenario,
                                     deferral_effect = deferral_effect,
                                     idu_type = idu_type,
                                     deferral_effect_year = deferral_effect_year,
                                     calibration_prevalence_1991 = calibration_prevalence_1991,
                                     donor_non_clearance = donor_non_clearance,
                                     contamination_constant = contamination_constant,
                                     trans_survival_years = trans_survival_years,
                                     survivor_non_clearance = survivor_non_clearance, 
                                     hepc_survival_hazard = hepc_survival_hazard){
  
  
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
  
  if(contamination_constant>calibration_prevalence_1991){
    stop("contamination_constant must be less than or equal to calibration_prevalence_1991.")
  }
  
  if(trans_survival_hazard_scenario !="basic" & trans_survival_hazard_scenario!="age"){
    stop("trans_survival_hazard_scenario must equal 'basic' or 'age'.")
  }
  
  
  
  # Table 1: prevalence of HCV donors 
  idu_calibration_point <- (calibration_prevalence_1991 - contamination_constant) # *donor_non_clearance # don't have this yet in probabilistic
  non_idu_constant <- contamination_constant
  

  if(nation == "Scotland"){
    table1 <- read_excel("Model/data/data_for_R_model.xlsx",
                         sheet="infected_IDUs_nations") %>% 
      filter(country==nation,
             injector_status==idu_type) %>%
      mutate(prop_of_1991 = no_hcv_infected_idus/last(no_hcv_infected_idus),
             prevalence_idu = ifelse(year>=deferral_effect_year,
                                     prop_of_1991*idu_calibration_point,
                                     prop_of_1991*idu_calibration_point*deferral_effect),
             prevalence = prevalence_idu + non_idu_constant) %>% data.frame()
  }
  
  
  if(nation %in% c("England", "Wales", "Northern Ireland") & idu_type == "ever") {
    table1 <- read.csv("Model/data/ever_hcv_idus.csv")
  } else if (nation %in% c("England", "Wales", "Northern Ireland") & idu_type == "current") {
    table1 <- read.csv("Model/data/current_hcv_idus.csv")
  } else if (nation %in% c("England", "Wales", "Northern Ireland") & idu_type == "past") {
    table1 <- read.csv("Model/data/past_hcv_idus.csv")
  } 
  
  if(nation %in% c("England", "Wales", "Northern Ireland")){
    idus <-  sample(size = 1, table1[-1], replace = TRUE)
    colnames(idus) <- "no_hcv_infected_idus"
    
    table1<- cbind(table1 %>% dplyr::select(year),idus)  %>%
      mutate(prop_of_1991 = no_hcv_infected_idus/last(no_hcv_infected_idus),
             prevalence_idu = ifelse(year>=deferral_effect_year,
                                     prop_of_1991*idu_calibration_point,
                                     prop_of_1991*idu_calibration_point*deferral_effect),
             prevalence = prevalence_idu + non_idu_constant) %>% data.frame()
      }
  

  # Table 2: number of units transfusions 
  
  table2 <- read_excel("Model/data/data_for_R_model.xlsx",sheet="number_of_donations_nations") %>%
    rename(units_used_det = units_used_per_donation) %>%
    as.data.frame() %>%
    filter(country == nation, source == units_per_donation_scenario)
  
  if(units_per_donation_scenario == "NBTSS"){
  table2[, "ci_width"] <- table2[,"upper"] - table2[,"lower"]
  table2[, "sd_est"] <- table2[,"ci_width"]/(1.96*2)
  table2[, "units_used_per_donation"] <- rnorm(n = nrow(table2), mean = table2[,"units_used_det"],
                                               sd = table2[,"sd_est"])
  } else {
  
  if(units_per_donation_scenario == "S&G"){
    table2[,"units_per_don"] <- rep(rpert(n = 1, min = 1.125, mode = 1.25, max = 1.6),22) 
    table2[,"prop_transfused"] <- rep(rpert(n = 1, min = 0.5, mode = 0.56, max = 0.66),22)
    table2[, "units_used_per_donation"] <- table2[,"units_per_don"]*table2[,"prop_transfused"]
  }
  }
  
  table2 <- table2 %>%
    mutate(ab_pos_donations = rbinom(n = nrow(table1), size = donations, prob = table1$prevalence/100)) %>%
    mutate(rna_pos_donations = rbinom(n = nrow(table1), size = ab_pos_donations, prob = donor_non_clearance))%>%
    mutate(rna_pos_units_transfused = round(rna_pos_donations * units_used_per_donation )) 
  
  
  # Table 3: number of people infected
  table3 <- merge(table1 %>% dplyr::select(year, prevalence),
                  table2 %>% dplyr::select(year, rna_pos_units_transfused),
                  all = TRUE) %>%
    rename(number_infected = rna_pos_units_transfused) 
  
  table3[,"number_chronic_infected"] <- rbinom(n = nrow(table3), size = table3[,"number_infected"],
                                               prob = survivor_non_clearance)
  
   
  # Table 4: age and sex of recipients 
  age_sex_recipients <- read_excel("Model/data/data_for_R_model.xlsx",sheet="age_sex_recipients") %>%
    rename(det_prop = proportion)
  
  age_sex_recipients[,"proportion"] <- t(mc2d::rdirichlet(1, alpha = age_sex_recipients$number))
  age_sex_recipients <- dplyr::select(age_sex_recipients, c(transfusion_age, sex, proportion))
  
  sum_recips <- sum(age_sex_recipients$proportion)

  table4 <- c()
  
  for(year in 1:length(table3$year)){
    
    fix_seed <- round(runif(1, min = 1, max = 1e6))
    set.seed(fix_seed) # this is to ensure the multinomial is the same for infected and chronic.
    recips <- age_sex_recipients %>% mutate(number_infected = 
                                              round(rmultinom(n = 1, size = table3$number_infected[year], 
                                                              prob = age_sex_recipients$proportion)),
                                            transfusion_year = table3$year[year])
    set.seed(fix_seed)
    # print(fix_seed)
    recips <-  recips %>% mutate(number_chronic_infected = 
                                   round(rmultinom(n = 1, size = table3$number_chronic_infected[year], 
                                                   prob = age_sex_recipients$proportion)))
    
    if(sum(recips$number_infected) != table3$number_infected[year]) {
      print("warning -  number infected doesn't match")
    }
    table4 <- rbind(table4,recips)
  }
  
  
  #print("Table 4")
  
  # Table 5: post-transfusion survival
  post_transfusion_survival <- read_excel("Model/data/data_for_R_model.xlsx",
                                          sheet="post_transfusion_survival") %>% 
    filter(survival_years==trans_survival_years) %>%
    rename(det_surv = survival)
  
  eff_sample_size <- read.csv("Model/data/effective_sample_size.csv") %>%
    dplyr::select(transfusion_age, sex, total)
  
  post_transfusion_survival <- merge(post_transfusion_survival, eff_sample_size) %>%
    mutate(num_alive = round(total*det_surv)) %>%
    mutate(num_dead = total - num_alive) %>%
    mutate(survival = rbeta(n = nrow(post_transfusion_survival), shape1 =  num_alive, shape2 =  num_dead))
  
  
  table5 <- merge(table4,post_transfusion_survival %>% dplyr::select(transfusion_age,sex,survival),
                  by=c("transfusion_age","sex"),all=TRUE)
  
  
  seed_fix_2 <- round(runif(n = 1, min = 0, max = 1e6))
  set.seed(seed_fix_2)
  table5[,"number_infected_survived_transfusion"] =  rbinom(n = nrow(table5), size = table5[,"number_infected"], prob = table5[, "survival"])
  set.seed(seed_fix_2)
  table5[,"number_chronic_infected_survived_transfusion"] =  rbinom(n = nrow(table5), size = table5[,"number_chronic_infected"], prob = table5[, "survival"])
  
  
  for (k in 1:nrow(table5)) {
    if(table5[k,"number_infected_survived_transfusion"] > table5[k,"number_infected"]) {
      print("warning - more survivors than recipients")
    } else {
      if(table5[k,"number_chronic_infected_survived_transfusion"] > table5[k,"number_chronic_infected"]) {
        print("warning - more survivors than recipients")
      }}}
  
  #print("Table 5")
  
  hazards_raw <- readRDS(paste0("Task 5/outputs/hazards_",nation,".RDS")) %>% 
    mutate(years_since_transfusion = survival_year - transfusion_year)
  
  if(trans_survival_hazard_scenario=="basic"){
    transfusion_hazards <- read_excel("Model/data/data_for_R_model.xlsx",
                                      sheet="transfusion_hazards_basic")
  } else{
    transfusion_hazards <- read_excel("Model/data/data_for_R_model.xlsx",
                                      sheet="transfusion_hazards_age")
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
  
  
  years_to_survive_to <- c(2019)
  
  table6 <- c()
  
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
             number_infected_survived_long_all = round(number_infected_survived_transfusion*survival_prob),
             number_infected_survived_long_trans = round(number_infected_survived_transfusion*survival_prob_trans),
             number_chronic_infected_survived_long_all = round(number_chronic_infected_survived_transfusion*survival_prob),
             number_chronic_infected_survived_long_hepc = round(number_chronic_infected_survived_transfusion*survival_prob_hepc),
             number_chronic_infected_survived_long_trans = round(number_chronic_infected_survived_transfusion*survival_prob_trans),
             number_chronic_infected_survived_long_hepc_trans = round(number_chronic_infected_survived_transfusion*survival_prob_hepc_trans),
             died_trans = number_chronic_infected - number_chronic_infected_survived_long_trans,
             died_trans_hcv = number_chronic_infected - number_chronic_infected_survived_long_hepc_trans,
             diff_died_trans_hepc = (number_chronic_infected - number_chronic_infected_survived_long_hepc_trans) - (number_chronic_infected - number_chronic_infected_survived_long_trans),
             died_post_trans_survival = died_trans + diff_died_trans_hepc - (number_chronic_infected - number_chronic_infected_survived_transfusion),
             excess_deaths_hcv = round(died_post_trans_survival*(1-(1/hepc_survival_hazard))))
    
    
    table6 <- rbind(table6,table6_it)
    
    
  }
  
  
  
  #### summarised output 
  # number infected, number infected 1970 - 1979; 1980 - 1991; survived 10 years; survived to end of period as specified in model 
  # list all the parameters included so can compare and rbind them together 
  # outputs with different hazards

  ## summary output
  output_summary <- data.frame(nation = nation,
                               deferral_effect = deferral_effect,
                               idu_type = idu_type,
                               deferral_effect_year = deferral_effect_year,
                               calibration_prevalence_1991 = calibration_prevalence_1991,
                               donor_non_clearance = donor_non_clearance,
                               contamination_constant = contamination_constant,
                               units_per_donation_scenario = units_per_donation_scenario,
                               trans_survival_years = trans_survival_years,
                               year_to_survive_to = year_to_survive_to,
                               hepc_survival_hazard = hepc_survival_hazard,
                               trans_survival_hazard_scenario = trans_survival_hazard_scenario,
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
             nation = nation,
           deferral_effect = deferral_effect,
           idu_type = idu_type,
           deferral_effect_year = deferral_effect_year,
           calibration_prevalence_1991 = calibration_prevalence_1991,
           donor_non_clearance = donor_non_clearance,
           contamination_constant = contamination_constant,
           units_per_donation_scenario = units_per_donation_scenario,
           trans_survival_years = trans_survival_years,
           year_to_survive_to = year_to_survive_to,
           hepc_survival_hazard = hepc_survival_hazard,
           survivor_non_clearance = survivor_non_clearance,
           trans_survival_hazard_scenario = trans_survival_hazard_scenario,
           survivor_non_clearance = survivor_non_clearance)
  
  ## gives the numbers from the table so hopefully don't have to think about it
  output_sens_table <- output_survival %>%
    mutate(infected_died_long_trans = total_infected - infected_survived_long_trans) %>%
    dplyr::select(nation,year_to_survive_to,deferral_effect,idu_type,deferral_effect_year,
                  calibration_prevalence_1991,
                  donor_non_clearance,contamination_constant,units_per_donation_scenario,trans_survival_years,
                  hepc_survival_hazard,survivor_non_clearance,trans_survival_hazard_scenario,
                  total_infected,total_chronic_infected,
                  total_chronic_survived_transfusion,
                  chronic_infected_survived_long_hepc_trans,
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
           deferral_effect = deferral_effect,
           idu_type = idu_type,
           deferral_effect_year = deferral_effect_year,
           calibration_prevalence_1991 = calibration_prevalence_1991,
           donor_non_clearance = donor_non_clearance,
           contamination_constant = contamination_constant,
           units_per_donation_scenario = units_per_donation_scenario,
           trans_survival_years = trans_survival_years,
           year_to_survive_to = year_to_survive_to,
           hepc_survival_hazard = hepc_survival_hazard,
           survivor_non_clearance = survivor_non_clearance,
           trans_survival_hazard_scenario = trans_survival_hazard_scenario,
           survivor_non_clearance = survivor_non_clearance)
  
  
  return(list(table1 = table1,
              table2 = table2,
              table3 = table3,
              table4 = table4,
              table5 = table5,
              table6 = table6,
              summary = output_summary,
              survival_summary = output_survival,
              sens_table = output_sens_table,
              survivors_sex = survivors_sex))
  
}





