### life tables for Scotland

library(tidyverse)
library(readxl)
library(reshape2)

# parameters
post_transfusion_survival_years <- 0
survival_year <- 2019

# options 
sex <- c("female","male")
age_groups <- c("0","1 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59","60 - 69",
                "70 - 79","80+")
transfusion_years <- c(1999,2004,2009,2014)
survival_years <- seq(min(transfusion_years)+post_transfusion_survival_years,survival_year,1)

combinations <- expand.grid("sex"=sex,"transfusion_age"=age_groups,"transfusion_year"=transfusion_years,
                            "survival_year"=survival_years)


### survival year has to be greater or equal to transfusion year

combinations <- combinations %>% filter(survival_year>=transfusion_year)


# midpoint of the age group at transfusion
combinations <- combinations %>% mutate(transfusion_age_midpoint = ifelse(transfusion_age=="1 - 9",5,
                                                                          ifelse(transfusion_age=="10 - 19",15,
                                                                                 ifelse(transfusion_age=="20 - 29",25,
                                                                                        ifelse(transfusion_age=="30 - 39",35,
                                                                                               ifelse(transfusion_age=="40 - 49",45,
                                                                                                      ifelse(transfusion_age=="50 - 59",55,
                                                                                                             ifelse(transfusion_age=="60 - 69",
                                                                                                                    65,
                                                                                                                    ifelse(transfusion_age=="70 - 79",
                                                                                                                           75,
                                                                                                                           ifelse(transfusion_age=="80+",
                                                                                                                                  90,ifelse(transfusion_age=="0",0,NA)))))))))))

# starting age at survival
combinations <- combinations %>% mutate(starting_age_survival = transfusion_age_midpoint+post_transfusion_survival_years)

# year difference must be at least 10
# changing name as filtering down
#combinations_years <- combinations %>% filter((survival_year-transfusion_year)>=10)

# what lifetable do we want to use
# 1979 - 1981 doesn't exist so we manually direct it to 1980-1982
# same with 2019 - 2021
combinations_years <- combinations %>% mutate(life_table = paste0(survival_year-1,"-",survival_year+1),
                                                    life_table = ifelse(life_table=="1979-1981","1980-1982",life_table),
                                                    life_table = ifelse(life_table=="2019-2021","2017-2019",life_table),
                                                    life_table = ifelse(life_table=="2018-2020","2017-2019",life_table)) 

# what age will the individuals be in the survival year
combinations_years <- combinations_years %>% mutate(age = (survival_year-transfusion_year)+transfusion_age_midpoint) %>% 
  arrange(transfusion_year)

# sense check, all good
combinations_years %>% filter(sex=="female",transfusion_year==1999,transfusion_age=="1 - 9")

# remove ages over 100 as will automatically go to 0
# try without this and see what happens
#combinations_years_age <- combinations_years %>% filter(age<=100)

combinations_years_age <- combinations_years


# iterate by life table as this will depend on a different sheet being read in 

hazards_per_combo <- c()


for(lt in unique(combinations_years_age$life_table)){
  
  data_lt <- read_excel("Task 5/data/nationallifetables3yrsco.xlsx",sheet=lt,skip=5)
  
  for(s in c("female","male")){
    
    hazards_it <- combinations_years_age %>% filter(life_table==lt,
                                                    sex==s)
    if(s=="female"){
      data_lt_s <- data_lt[,c(8,10)]
      colnames(data_lt_s) <- c("age","qx")
    } else if(s=="male"){
      data_lt_s <- data_lt[,c(1,3)]
      colnames(data_lt_s) <- c("age","qx")
    }
    
    hazards_it_merge <- merge(hazards_it,data_lt_s,by="age",all.x=TRUE)
    
    hazards_per_combo <- rbind(hazards_per_combo,hazards_it_merge)
    
  }
  
  
}


### this is then where to split to the different survival and transfusion years

survival_5 <- hazards_per_combo %>% mutate(sx = 1-qx) %>% filter(survival_year<=transfusion_year+5) %>%
  group_by(sex,transfusion_age,transfusion_year,transfusion_age_midpoint) %>%
  summarise(survival_5 = ifelse(transfusion_year+5<=2019,prod(sx),NA)) %>% unique()


survival_10 <- hazards_per_combo %>% mutate(sx = 1-qx) %>% filter(survival_year<=transfusion_year+10) %>%
  group_by(sex,transfusion_age,transfusion_year,transfusion_age_midpoint) %>%
  summarise(survival_10 = ifelse(transfusion_year+10<=2019,prod(sx),NA)) %>% unique()

survival_15 <- hazards_per_combo %>% mutate(sx = 1-qx) %>% filter(survival_year<=transfusion_year+15) %>%
  group_by(sex,transfusion_age,transfusion_year,transfusion_age_midpoint) %>%
  summarise(survival_15 = ifelse(transfusion_year+15<=2019,prod(sx),NA)) %>% unique()

survival_20 <- hazards_per_combo %>% mutate(sx = 1-qx) %>% filter(survival_year<=transfusion_year+20) %>%
  group_by(sex,transfusion_age,transfusion_year,transfusion_age_midpoint) %>%
  summarise(survival_20 = ifelse(transfusion_year+20<=2019,prod(sx),NA)) %>% unique()




survival_per_survival_year <- merge(merge(merge(survival_5,survival_10),survival_15),survival_20)
write.csv(survival_per_survival_year,"Task 5/outputs/scotland_survival_raw_per_survival_year.csv",row.names=FALSE)


### now lets calculate the between epoch survivals 

hazards_per_combo %>% head()

#### think need to split this multiple times and do 4 groupings 

### matches what we have above for 1999 - just need to repeat this for the different survival bands 

survival_0_5 <- hazards_per_combo %>% mutate(sx = 1-qx) %>% filter(survival_year>=transfusion_year,survival_year<transfusion_year+5) %>%
  group_by(sex,transfusion_age,transfusion_year,transfusion_age_midpoint) %>% 
    summarise(survival_0_5 = ifelse(transfusion_year+5<=2019,prod(sx),NA)) %>% unique()


survival_5_10 <- hazards_per_combo %>% mutate(sx = 1-qx) %>% filter(survival_year>=transfusion_year+5,survival_year<transfusion_year+10) %>%
  group_by(sex,transfusion_age,transfusion_year,transfusion_age_midpoint) %>% 
  summarise(survival_5_10 = ifelse(transfusion_year+10<=2019,prod(sx),NA)) %>% unique()


survival_10_15 <- hazards_per_combo %>% mutate(sx = 1-qx) %>% filter(survival_year>=transfusion_year+10,survival_year<transfusion_year+15) %>%
  group_by(sex,transfusion_age,transfusion_year,transfusion_age_midpoint) %>% 
  summarise(survival_10_15 = ifelse(transfusion_year+15<=2019,prod(sx),NA)) %>% unique()


survival_15_20 <- hazards_per_combo %>% mutate(sx = 1-qx) %>% filter(survival_year>=transfusion_year+15,survival_year<transfusion_year+20) %>%
  group_by(sex,transfusion_age,transfusion_year,transfusion_age_midpoint) %>% 
  summarise(survival_15_20 = ifelse(transfusion_year+20<=2019,prod(sx),NA)) %>% unique()



survival_per_epoch <- merge(merge(merge(survival_0_5,survival_5_10,all.x=TRUE),survival_10_15,all.x=TRUE),survival_15_20,all.x=TRUE)
write.csv(survival_per_epoch,"Task 5/outputs/scotland_survival_raw_per_epoch.csv",row.names=FALSE)



### read in what was observed to be able to compare

#### lets create a susceptibles data frame

transfused <- read_excel("Task 5/data/survivors.xlsx",sheet="recipients_by_cohort_year") #%>%
transfused <- transfused %>%
  filter(transfusion_year==1999|transfusion_year==2004) %>% 
  rename(transfused = number)

emigrations <- read_excel("Task 5/data/survivors.xlsx",sheet = "emigrations") %>% 
  filter(survival_time==5) %>%
  rename(emigrations_time_5 = number) %>%
  mutate(emigrations_time_5 = ifelse(is.na(emigrations_time_5),0,emigrations_time_5))


susceptibles_5 <- merge(transfused,emigrations) %>% 
  mutate(susceptible = transfused - emigrations_time_5)



## get the susceptibles for 10, 15, 20 years directly from the excel sheet


survivors <- read_excel("Task 5/data/survivors.xlsx",sheet="survivors") %>%
  filter(survival_time!=20) %>%
  rename(susceptible = number) %>%
  mutate(survival_time = survival_time + 5) ### this is because we take susceptibles as the period before


susceptibles <- rbind(susceptibles_5 %>% 
                        dplyr::select(transfusion_year,sex,transfusion_age,survival_time,susceptible),
                      survivors)



#### now we need to add in the probability of survival - this is more straightforward as no mucking about with different rules 

survival_per_epoch <- melt(read.csv("Task 5/outputs/scotland_survival_raw_per_epoch.csv"),
                           id.vars=c("sex","transfusion_age","transfusion_year","transfusion_age_midpoint")) %>% 
  rename(probability = value)

survival <- survival_per_epoch %>% 
  mutate(survival_time = ifelse(variable=="survival_0_5",5,
                                ifelse(variable=="survival_5_10",10,
                                       ifelse(variable=="survival_10_15",15,
                                              ifelse(variable=="survival_15_20",20,NA))))) %>%
  filter(transfusion_year==1999|transfusion_year==2004)



expected <- merge(susceptibles,survival,all=TRUE) %>% arrange(transfusion_year,survival_time) %>%
  mutate(expected_survivors = susceptible * probability,
         expected_deaths = susceptible - expected_survivors)


expected %>% filter(sex=="female",transfusion_age=="10 - 19")
expected %>% filter(sex=="female",transfusion_age=="50 - 59")





obs_transfusions <- read_excel("Task 5/data/survivors.xlsx",sheet="recipients_by_cohort_year")
obs_deaths <- read_excel("Task 5/data/survivors.xlsx",sheet="deaths")
obs_emi <- read_excel("Task 5/data/survivors.xlsx",sheet="emigrations")


#######

obs_transfusions_f <- obs_transfusions %>% filter(transfusion_year==1999|transfusion_year==2004)

obs_deaths_f <- dcast(obs_deaths %>% 
                        mutate(number = ifelse(is.na(number),0,number)),
                     transfusion_age+sex+transfusion_year~survival_time) %>%
  rename(deaths_5 = "5",
         deaths_10 = "10",
         deaths_15 = "15",
         deaths_20 = "20")


obs_emi_f <- dcast(obs_emi %>% 
                     mutate(number = ifelse(is.na(number),0,number)),
                   transfusion_age+sex+transfusion_year~survival_time) %>%
  rename(emi_5 = "5",
         emi_10 = "10",
         emi_15 = "15",
         emi_20 = "20")


obs <- merge(obs_transfusions_f %>%
               rename(transfusions = number),
             merge(obs_deaths_f,obs_emi_f,by=c("transfusion_age","sex","transfusion_year"))) %>% 
  mutate(survival_5 = transfusions - deaths_5 - emi_5,
         survival_10 = survival_5 - deaths_10 - emi_10,
         survival_15 = survival_10 - deaths_15 - emi_15,
         survival_20 = survival_15 - deaths_20 - emi_20) %>% 
  arrange(transfusion_year)


### how many died from both the cohorts

obs %>% group_by(transfusion_year) %>% 
  summarise(deaths = sum(deaths_5,deaths_10,deaths_15,deaths_20))




### and now merge with observed

deaths_observed <- read_excel("Task 5/data/survivors.xlsx",sheet="deaths") %>%
  mutate(number = ifelse(is.na(number),0,number)) %>%
  rename(observed_deaths = number)


obs_exp <- merge(expected,deaths_observed) %>% arrange(transfusion_year,survival_time) %>%
  mutate(obs_to_exp = observed_deaths/expected_deaths)
write.csv(obs_exp,"Task 5/outputs/scotland_observed_expected_deaths.csv",row.names = FALSE)











