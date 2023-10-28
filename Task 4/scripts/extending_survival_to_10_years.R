## wallis survival modelling 
## extending 5 year to 10 year survival using EASTR study

library(tidyverse)
library(readxl)
library(survival)


#### updated data

updated_data <- read_excel("Task 4/data/Transfusion study updated.xls") %>% data.frame()

# only want data with known age

updated_data <- updated_data %>% filter(!is.na(Age))

## want overall 1,5 & 7 year survival


km_all_updated <- survfit(Surv(Survival,Code)~1,
                          data = updated_data)


plot(km_all_updated)

## 1 year survival
km_all_updated$surv[first(which(km_all_updated$time>=12))]

## 1 year survival
km_all_updated$surv[first(which(km_all_updated$time>=60))]

## 1 year survival
km_all_updated$surv[first(which(km_all_updated$time>=84))]



### now want 1, 5, 7 year survival by age groupings of EASTR


# 16 - 24
# 25 - 39
# 40 - 59
# 60 - 74
# 75+
  
  

updated_data <- updated_data %>% mutate(Age_group = ifelse(Age>=16&Age<=24,"16 - 24",
                                                           ifelse(Age>=25&Age<=39,"25 - 39",
                                                                  ifelse(Age>=40&Age<=59,"40 - 59",
                                                                         ifelse(Age>=60&Age<=74,"60 - 74",
                                                                                ifelse(Age>=75,"75+",NA))))))


updated_data %>% filter(is.na(Age_group)) %>% nrow() #146

table(updated_data$Age_group)


## do by age first
survival_probs_age_updated <- updated_data %>% filter(!is.na(Age_group)) %>% dplyr::select(Age_group) %>% unique() %>%
  mutate(one_year_surv = NA,
         five_year_surv = NA,
         seven_year_surv = NA) %>% arrange(Age_group)

for(a in 1:nrow(survival_probs_age_updated)){
  # age group corresponding to iteration
  age_group_it <- survival_probs_age_updated$Age_group[a]
  # subset data to that age group
  data_it <- updated_data %>% filter(Age_group==age_group_it)
  # kaplan meier model
  km_it <- survfit(Surv(Survival,Code)~1,
                   data = data_it)
  # get 5 probability of surviving more than 5 years
  survival_probs_age_updated$one_year_surv[a] <- km_it$surv[first(which(km_it$time>=12))]
  survival_probs_age_updated$five_year_surv[a] <- km_it$surv[first(which(km_it$time>=60))]
  survival_probs_age_updated$seven_year_surv[a] <- km_it$surv[first(which(km_it$time>=84))]
}

round(survival_probs_age_updated$one_year_surv,3)
round(survival_probs_age_updated$five_year_surv,3)
round(survival_probs_age_updated$seven_year_surv,3)



#### extending Wallis to 10 years
 # per age group in EASTR, calculate ratio of log survival times at 10 and 5 years

r1 <- log(0.883)/log(0.898)
r2 <- log(0.813)/log(0.837)
r12 <- mean(r1,r2)
r3 <- log(0.535)/log(0.603)
r4 <- log(0.357)/log(0.471)
r5 <- log(0.124)/log(0.275)

## 0 - 9
0.73^r12
0.83^r12
## 10 - 19
0.84^r12
0.61^r12
## 20 - 29
0.97^r12
0.82^r12
## 30 - 39
0.80^r12
0.73^r12
## 40 - 49
0.61^r3
0.61^r3
## 50 - 59
0.52^r3
0.47^r3
## 60 - 69
0.49^r4
0.46^r4
## 70 - 79
0.43^r4
0.28^r4
## 80 - 89
0.24^r5
0.14^r5
## 90+
0.09^r5
0.09^r5




