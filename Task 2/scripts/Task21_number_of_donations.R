## task 2.1 - number of donations

library(tidyverse)
library(readxl)
library(cowplot)


## first regression model on 1978 - 1990 to get data for 1991

### plot the regression
donations_raw_80s <- data.frame("year" = seq(1978,1991,1),
                            "donations" = c(2123607,2144484,2220036,2065428,
                                            2058994,2135840,2158626,2119060,
                                            2128450,2094316,2140810,2225009,
                                            2180858,NA))


observed_data_80s <- data.frame("year" = seq(1978,1990),
                            donations = c(2123607,2144484,2220036,2065428,
                                          2058994,2135840,2158626,2119060,
                                          2128450,2094316,2140810,2225009,
                                          2180858))


# poisson regression
poisson_ew_80s <- glm(donations~year,data = observed_data_80s,
                  family="poisson")
#summary(poisson_ew)

resid_dev_80s <- poisson_ew_80s$deviance
mean_obs_year_80s <- mean(observed_data_80s$year)
denom_80s <- (observed_data_80s$year-mean_obs_year_80s)^2 %>% sum()

poisson_ew_predict_80s <- predict(poisson_ew_80s,type="response",se.fit=TRUE,
                              newdata = data.frame("year"=seq(1978,1991)))

donations_80s <- data.frame("year"=seq(1978,1991),
                        "predicted"=poisson_ew_predict_80s$fit) %>% 
  mutate(syhat = resid_dev_80s*sqrt((1/nrow(observed_data_80s))+(((year - mean_obs_year_80s)^2)/(denom_80s))),
         lower_ci = predicted - qt(0.975,df=nrow(observed_data_80s))*syhat,
         upper_ci = predicted + qt(0.975,df=nrow(observed_data_80s))*syhat,
         lower_pi = predicted - qt(0.975,df=nrow(observed_data_80s))*sqrt(resid_dev_80s^2 + syhat^2),
         upper_pi = predicted + qt(0.975,df=nrow(observed_data_80s))*sqrt(resid_dev_80s^2 + syhat^2),
         observed = donations_raw_80s$donations)

ggplot(donations_80s)+
  geom_rect(aes(xmin=1990.5,xmax=1991.5,ymin=-Inf,ymax=Inf),fill="lightgrey")+
    geom_line(aes(x=year,y=predicted,linetype="Model fit"))+
  geom_errorbar(data = donations_80s %>% filter(is.na(observed)),
                aes(x=year,ymin=lower_pi,ymax=upper_pi,col="Prediction interval"))+
  geom_point(data = donations_80s %>% filter(year<1978|year==1991),
             aes(x=year,y=predicted,col="Estimated using our model"))+
  geom_point(aes(x=year,y=observed,col="Raw data"))+
  scale_colour_manual(values=c("blue","blue","black"))+
  scale_fill_manual(values="blue")+
  scale_linetype_manual(values="dashed")+
  labs(x="Year",y="Number of donations",col="",fill="",linetype="")+
  scale_x_continuous(breaks=seq(min(donations_80s$year),max(donations_80s$year),1))+
  scale_y_continuous(limits=c(min(donations_80s$predicted) - (2.5*(max(donations_80s$predicted-min(donations_80s$predicted)))),
                               max(donations_80s$predicted) + (2.5*(max(donations_80s$predicted-min(donations_80s$predicted))))),
                      labels=scales::comma)+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle=90),
        panel.grid.minor.x = element_blank(),
        legend.box = "vertical")+
  guides(col = guide_legend(order=1,
                            override.aes = list(linetype = c(0, 1, 0),
                                                shape = c(16,NA,16)) ))


## second regression model for the 70s

donations_raw_70s <- data.frame("year" = seq(1970,1979,1),
                                "donations" = c(NA,NA,NA,NA,NA,1780000,1962000,NA,2123607,2144484))

observed_data_70s <- data.frame("year" = c(1975,1976,1978,1979),
                                "donations" = c(1780000,1962000,2123607,2144484))


poisson_ew_70s <- glm(donations~year,data = observed_data_70s,
                      family="poisson")


resid_dev_70s <- poisson_ew_70s$deviance
mean_obs_year_70s <- mean(observed_data_70s$year)
denom_70s <- (observed_data_70s$year-mean_obs_year_70s)^2 %>% sum()

poisson_ew_predict_70s <- predict(poisson_ew_70s,type="response",se.fit=TRUE,
                                  newdata = data.frame("year"=seq(1970,1979)))


donations_70s <- data.frame("year"=seq(1970,1979),
                            "predicted"=poisson_ew_predict_70s$fit) %>% 
  mutate(syhat = resid_dev_70s*sqrt((1/nrow(observed_data_70s))+(((year - mean_obs_year_70s)^2)/(denom_70s))),
         lower_ci = predicted - qt(0.975,df=nrow(observed_data_70s))*syhat,
         upper_ci = predicted + qt(0.975,df=nrow(observed_data_70s))*syhat,
         lower_pi = predicted - qt(0.975,df=nrow(observed_data_70s))*sqrt(resid_dev_70s^2 + syhat^2),
         upper_pi = predicted + qt(0.975,df=nrow(observed_data_70s))*sqrt(resid_dev_70s^2 + syhat^2),
         observed = donations_raw_70s$donations)


donations_70s %>% 
  mutate(syhat_new = (resid_dev_70s*sqrt(1+(1/nrow(observed_data_70s))+(((year - mean_obs_year_70s)^2)/(denom_70s)))),
         lower_pi_new = predicted - qt(0.975,df=nrow(observed_data_70s))*sqrt(resid_dev_70s^2 + syhat_new^2))


ggplot(donations_70s)+
  geom_rect(aes(xmin=1969.5,xmax=1974.5,ymin=-Inf,ymax=Inf),fill="lightgrey")+
  geom_rect(aes(xmin=1976.5,xmax=1977.5,ymin=-Inf,ymax=Inf),fill="lightgrey")+
  #geom_rect(aes(xmin=1990.5,xmax=1991.5,ymin=-Inf,ymax=Inf),fill="lightgrey")+
  #geom_ribbon(aes(x=year,ymin=lower,ymax=upper,fill="Model uncertainty"),alpha=0.2)+
  geom_line(aes(x=year,y=predicted,linetype="Model fit"))+
  geom_errorbar(data = donations_70s %>% filter(is.na(observed)),
                aes(x=year,ymin=lower_pi,ymax=upper_pi,col="Prediction interval"))+
  geom_point(data = donations_70s %>% filter(is.na(observed)),
             aes(x=year,y=predicted,col="Estimated using our model"))+
  geom_point(aes(x=year,y=observed,col="Raw data"))+
  scale_colour_manual(values=c("blue","blue","black"))+
  scale_fill_manual(values="blue")+
  scale_linetype_manual(values="dashed")+
  labs(x="Year",y="Number of donations",col="",fill="",linetype="")+
  scale_x_continuous(breaks=seq(min(donations_70s$year),max(donations_70s$year),1))+
  scale_y_continuous(limits=c(min(donations_70s$predicted) - (0.5*(max(donations_70s$predicted-min(donations_70s$predicted)))),
                              max(donations_70s$predicted) + (0.5*(max(donations_70s$predicted-min(donations_70s$predicted))))),
                     labels=scales::comma)+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle=90),
        panel.grid.minor.x = element_blank(),
        legend.box = "vertical")+
  guides(col = guide_legend(order=1,
                            override.aes = list(linetype = c(0, 1, 0),
                                                shape = c(16,NA,16))))


### combined plot

plot_grid(ggplot(donations_70s)+
            geom_rect(aes(xmin=1969.5,xmax=1974.5,ymin=-Inf,ymax=Inf),fill="lightgrey")+
            geom_rect(aes(xmin=1976.5,xmax=1977.5,ymin=-Inf,ymax=Inf),fill="lightgrey")+
            geom_line(aes(x=year,y=predicted,linetype="Model fit"))+
            geom_errorbar(data = donations_70s %>% filter(is.na(observed)),
                          aes(x=year,ymin=lower_pi,ymax=upper_pi,col="95% Prediction interval"))+
            geom_point(data = donations_70s %>% filter(is.na(observed)),
                       aes(x=year,y=predicted,col="Estimated using our model"))+
            geom_point(aes(x=year,y=observed,col="Raw data"))+
            scale_colour_manual(values=c("blue","blue","black"))+
            scale_fill_manual(values="blue")+
            scale_linetype_manual(values="dashed")+
            labs(x="Year",y="Number of donations",col="",fill="",linetype="",tag="A")+
            scale_x_continuous(breaks=seq(min(donations_70s$year),max(donations_70s$year),1))+
            scale_y_continuous(limits=c(min(donations_70s$predicted) - (0.25*(max(donations_70s$predicted-min(donations_70s$predicted)))),max(donations_70s$predicted) + (0.25*(max(donations_70s$predicted-min(donations_70s$predicted))))),
                               labels=scales::comma)+
            theme_bw()+
            theme(legend.position="none",
                  axis.text.x = element_text(angle=90),
                  panel.grid.minor.x = element_blank(),
                  legend.box = "vertical")+
            guides(col = guide_legend(order=1,
                                      override.aes = list(linetype = c(0, 1, 0),
                                                          shape = c(16,NA,16)))),
            ggplot(donations_80s)+
            geom_rect(aes(xmin=1990.5,xmax=1991.5,ymin=-Inf,ymax=Inf),fill="lightgrey")+
            geom_line(aes(x=year,y=predicted,linetype="Model fit"))+
            geom_errorbar(data = donations_80s %>% filter(is.na(observed)),
                          aes(x=year,ymin=lower_pi,ymax=upper_pi,col="95% Prediction interval"))+
            geom_point(data = donations_80s %>% filter(year<1978|year==1991),
                       aes(x=year,y=predicted,col="Estimated using our model"))+
            geom_point(aes(x=year,y=observed,col="Raw data"))+
            scale_colour_manual(values=c("blue","blue","black"))+
            scale_fill_manual(values="blue")+
            scale_linetype_manual(values="dashed")+
            labs(x="Year",y="Number of donations",col="",fill="",linetype="",tag="B")+
            scale_x_continuous(breaks=seq(min(donations_80s$year),max(donations_80s$year),1))+
            scale_y_continuous(limits=c(min(donations_80s$predicted) - (2.5*(max(donations_80s$predicted-min(donations_80s$predicted)))),max(donations_80s$predicted) + (2.5*(max(donations_80s$predicted-min(donations_80s$predicted))))),
                               labels=scales::comma)+
            theme_bw()+
            theme(legend.position="bottom",
                  axis.text.x = element_text(angle=90),
                  panel.grid.minor.x = element_blank(),
                  legend.box = "vertical")+
            guides(col = guide_legend(order=1,
                                      override.aes = list(linetype = c(1, 0, 0),
                                                          shape = c(NA,16,16)) )),
          nrow=2,rel_heights = c(1,1.3)
  )
ggsave("Task 2/outputs/SupFigure1.png",height=8,width=6)



### want the resulting combination/final figures

donations_combined <- rbind(donations_70s %>% filter(year<1978),
                            donations_80s %>% filter(year>=1978)) %>% 
  dplyr::select(year,predicted,observed) %>%
  mutate(value = ifelse(is.na(observed),predicted,observed),
         label = factor(ifelse(is.na(observed),"Modelled","Observed"),
                        levels=c("Observed","Modelled")),
         value = ifelse(year==1991,value*2/3,value))


ggplot(donations_combined,aes(x=year,y=value,fill=label))+
  geom_col()+
  theme_bw()+
  scale_fill_manual(values=c("black","grey"))+
  theme(axis.text.x = element_text(angle=90),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks=seq(min(donations_combined$year),max(donations_combined$year),1))+
  scale_y_continuous(labels=scales::comma)+
  labs(x="Year",y="Number of donations",fill="")
ggsave("Task 2/outputs/SupFigure2.png",height=4,width=5)

