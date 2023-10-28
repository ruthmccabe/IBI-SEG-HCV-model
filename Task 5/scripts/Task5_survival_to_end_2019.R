### get hazards from each of the four nations using their lifetables 

library(tidyverse)
library(readxl)
library(reshape2)


### combinations are the same for each of the four nations 

# parameters
post_transfusion_survival_years <- 10
survival_year <- 2019

# options 
sex <- c("female","male")
age_groups <- c("0 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59","60 - 69",
                "70 - 79","80 - 89","90+")
transfusion_years <- seq(1970,1991,1)
survival_years <- seq(min(transfusion_years)+post_transfusion_survival_years,survival_year,1)

combinations <- expand.grid("sex"=sex,"transfusion_age"=age_groups,"transfusion_year"=transfusion_years,
                            "survival_year"=survival_years)

# midpoint of the age group at transfusion
combinations <- combinations %>% 
  mutate(transfusion_age_midpoint = ifelse(transfusion_age=="0 - 9",5,
                                           ifelse(transfusion_age=="10 - 19",15,
                                           ifelse(transfusion_age=="20 - 29",25,
                                                  ifelse(transfusion_age=="30 - 39",35,
                                                  ifelse(transfusion_age=="40 - 49",45,
                                                         ifelse(transfusion_age=="50 - 59",55,
                                                         ifelse(transfusion_age=="60 - 69",65,
                                                                ifelse(transfusion_age=="70 - 79",75,
                                                                ifelse(transfusion_age=="80 - 89",85,
                                                                       ifelse(transfusion_age=="90+",95,NA)))))))))))

# starting age at survival
combinations <- combinations %>%
  mutate(starting_age_survival = transfusion_age_midpoint+post_transfusion_survival_years)

# year difference must be at least 10
combinations_years <- combinations %>% filter((survival_year-transfusion_year)>=post_transfusion_survival_years)

# 1979 - 1981 doesn't exist so we manually direct it to 1980-1982
# same with 2019 - 2021 and 2018 - 2020
combinations_years <- combinations_years %>% mutate(life_table = paste0(survival_year-1,"-",survival_year+1),
                                                    life_table = ifelse(life_table=="1979-1981","1980-1982",life_table),
                                                    life_table = ifelse(life_table=="2019-2021","2017-2019",life_table),
                                                    life_table = ifelse(life_table=="2018-2020","2017-2019",life_table)) 
# what age will the individuals be in the survival year
combinations_years <- combinations_years %>% 
  mutate(age = (survival_year-transfusion_year)+transfusion_age_midpoint) %>% 
  arrange(transfusion_year)

# sense check, all good
combinations_years %>% filter(sex=="female",transfusion_year==1970,transfusion_age=="0 - 9")

# so don't need to rewrite below
combinations_years_age <- combinations_years


hazards_home_nations <- function(nation,combinations_years_age){
  
  hazards_per_combo <- c()
  
  for(lt in unique(combinations_years_age$life_table)){
    
    if(nation=="Scotland"){
      data_lt <- read_excel("Task 5/data/nationallifetables3yrsco.xlsx",sheet=lt,skip=5)
    } else if(nation=="England"){
      data_lt <- read_excel("Task 5/data/nationallifetables3yreng.xlsx",sheet=lt,skip=5)
    } else if(nation=="Wales"){
      data_lt <- read_excel("Task 5/data/nationallifetables3yrwal.xlsx",sheet=lt,skip=5)
    } else if(nation=="Northern Ireland"){
      data_lt <- read_excel("Task 5/data/nationallifetables3yrni.xlsx",sheet=lt,skip=5)
    } else{
      stop("nation must be one of 'Scotland', 'England', 'Wales' or 'Northern Ireland'.")
    }
    
  
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
    
    saveRDS(hazards_per_combo,
            paste0("Task 5/outputs/hazards_",nation,".RDS"))
  

  
}
  
    return(hazards_per_combo)
}


england <- hazards_home_nations("England",combinations_years_age = combinations_years_age)

scotland <- hazards_home_nations("Scotland",combinations_years_age = combinations_years_age)

wales <- hazards_home_nations("Wales",combinations_years_age = combinations_years_age)

ni <- hazards_home_nations("Northern Ireland",combinations_years_age = combinations_years_age)









