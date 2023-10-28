### IDUs 1970

## ever injecting 
## from SMB
 
ratio <- 33324/23132 ##1975/1971
exp(log(ratio)/4)
23132/exp(log(ratio)/4)


## current injecting

ratio <- 19640/13382
exp(log(ratio)/4)
13382/exp(log(ratio)/4)


## past injecting

ratio <- 13360/9665
exp(log(ratio)/4)
9665/exp(log(ratio)/4)

### Sarah this might help you with the 1000 realisations


table1 <- read_excel("model/data/data_for_R_model.xlsx",
                     sheet="infected_IDUs")

## value for 1970s

table1  %>%
  filter(year==1971|year==1975) %>%
  group_by(injector_status) %>%
  summarise(value_71 = first(no_hcv_infected_idus),
            ratio = last(no_hcv_infected_idus)/first(no_hcv_infected_idus),
            step2 = exp(log(ratio)/4),
            value_70 = value_71/step2)



### do this for the lower and upper ever-IDU bounds
ratio <- 29363/20033
exp(log(ratio)/4)
20033/exp(log(ratio)/4)


ratio <- 36570/25979
exp(log(ratio)/4)
25979/exp(log(ratio)/4)



