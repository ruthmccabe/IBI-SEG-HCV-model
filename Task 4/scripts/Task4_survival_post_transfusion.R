## Survival modelling using updated data from Wallis et al.

library(tidyverse)
library(readxl)
library(survival)


updated_data <- read_excel("Task 4/data/Transfusion study updated.xls") %>% data.frame()


updated_data <- updated_data %>% 
  mutate(Age_group = ifelse(Age<10,"0 - 9",
                            ifelse(Age>=10&Age<20,"10 - 19",
                                   ifelse(Age>=20&Age<30,"20 - 29",
                                          ifelse(Age>=30&Age<40,"30 - 39",
                                                 ifelse(Age>=40&Age<50,"40 - 49",
                                                        ifelse(Age>=50&Age<60,"50 - 59",
                                                               ifelse(Age>=60&Age<70,"60 - 69",
                                                                      ifelse(Age>=70&Age<80,"70 - 79",
                                                                             ifelse(Age>=80&Age<90,"80 - 89",
                                                                                    ifelse(Age>=90,"90+",NA)))))))))))


updated_data %>% filter(is.na(Age_group))

table(updated_data$Age_group,updated_data$Sex) 

# get totals per sex and age group separately
table(updated_data$Age_group,updated_data$Sex) %>% colSums()
table(updated_data$Age_group,updated_data$Sex) %>% rowSums()


# total obs
table(updated_data$Age_group,updated_data$Sex)  %>% sum()

## just do a general KM curve with all data to figure out syntax

# eg assuming that 1 corresponds to dead 
#updated_data <- updated_data %>% mutate(status = ifelse(Alive.or.dead==1,TRUE,FALSE))


km_all_updated <- survfit(Surv(Survival,Code)~1,
                  data = updated_data)
plot(km_all_updated)
## how to extract 5 year survival probability 
km_all_updated$surv[first(which(km_all_updated$time>=120))]

## do by age first
survival_probs_age_updated <- updated_data %>% filter(!is.na(Age_group)) %>% dplyr::select(Age_group) %>% unique() %>%
  mutate(five_year_surv = NA) %>% arrange(Age_group)

for(a in 1:nrow(survival_probs_age_updated)){
  # age group corresponding to iteration
  age_group_it <- survival_probs_age_updated$Age_group[a]
  # subset data to that age group
  data_it <- updated_data %>% filter(Age_group==age_group_it)
  # kaplan meier model
  km_it <- survfit(Surv(Survival,Code)~1,
                   data = data_it)
  # get 5 probability of surviving more than 5 years 
  survival_probs_age_updated$five_year_surv[a] <- km_it$surv[first(which(km_it$time>=60))]
}
round(survival_probs_age_updated$five_year_surv,2)


## do by age & sex
survival_probs_age_sex_updated <- updated_data %>% filter(!is.na(Age_group)) %>% 
  dplyr::select(Age_group,Sex) %>% unique() %>%
  mutate(five_year_surv = NA,
         three_year_surv = NA,
         one_year_surv = NA) %>% arrange(Age_group)

for(a in 1:nrow(survival_probs_age_sex_updated)){
  # age group corresponding to iteration
  age_group_it <- survival_probs_age_sex_updated$Age_group[a]
  sex_it <- survival_probs_age_sex_updated$Sex[a]
  # subset data to that age group
  data_it <- updated_data %>% filter(Age_group==age_group_it,
                             Sex==sex_it)
  # kaplan meier model
  km_it <- survfit(Surv(Survival,Code)~1,
                   data = data_it)
  # get 5 probability of surviving more than 5 years 
  survival_probs_age_sex_updated$five_year_surv[a] <- km_it$surv[first(which(km_it$time>=60))]
  survival_probs_age_sex_updated$three_year_surv[a] <- km_it$surv[first(which(km_it$time>=36))]
  survival_probs_age_sex_updated$one_year_surv[a] <- km_it$surv[first(which(km_it$time>=12))]
}
## 5 year survival
survival_probs_age_sex_updated %>% mutate(round(five_year_surv,2)) %>% filter(Sex=="F")
survival_probs_age_sex_updated %>% mutate(round(five_year_surv,2)) %>% filter(Sex=="M")

## 3 year survival
survival_probs_age_sex_updated %>% mutate(round(three_year_surv,2)) %>% filter(Sex=="F")
survival_probs_age_sex_updated %>% mutate(round(three_year_surv,2)) %>% filter(Sex=="M")

## 1 year survival
survival_probs_age_sex_updated %>% mutate(round(one_year_surv,2)) %>% filter(Sex=="F")
survival_probs_age_sex_updated %>% mutate(round(one_year_surv,2)) %>% filter(Sex=="M")


### 10 year survival probability?

# what is the max survival for each age and sex category

updated_data %>% filter(!is.na(Age_group)) %>% group_by(Sex,Age_group) %>% 
  summarise(max_survival = max(Survival)) %>% data.frame()

### reduction in survival from 5 to 10 years based on EASTR study
reduction <- 47/36 - 1 #30.6%

survival_probs_age_sex_updated %>% 
  mutate(five_year_round = round(five_year_surv,2),
         ten_year_surv = five_year_surv *(1-reduction),
         ten_year_round = round(ten_year_surv,2)) %>% filter(Sex=="F")

survival_probs_age_sex_updated %>% 
  mutate(five_year_round = round(five_year_surv,2),
         ten_year_surv = five_year_surv *(1-reduction),
         ten_year_round = round(ten_year_surv,2)) %>% filter(Sex=="M")






