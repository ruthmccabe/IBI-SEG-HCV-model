## task 4.2: units transfused per donation

library(tidyverse)

# data from IBI
units_donation <- data.frame("year" = seq(1982,1988),
                             "units_used_per_donation" = c(0.99,1.02,1.07,1.11,1.15,1.19,1.20))

# linear regression
m1 <- lm(units_used_per_donation~year,data=units_donation)
summary(m1)

m1_pred <- predict(object = m1,newdata = data.frame("year"=seq(1970,1991,1)),
                   se.fit = TRUE,interval="prediction")

m1_conf <- predict(object = m1,newdata = data.frame("year"=seq(1970,1991,1)),
                   se.fit = TRUE,interval="confidence")

# ## 1970 currently around 0.55 
# ## 25% reduction in gradient resulting in 0.55 going to 0.85
# 
# units_donations_70s <- data.frame("year"=c(1970,1982),
#                                   "units_used_per_donation"=c(0.8,0.99))
# 
# m2 <- lm(units_used_per_donation~year,data=units_donations_70s)
# summary(m2)
# 
# m2_pred <- predict(object = m2,newdata = data.frame("year"=seq(1970,1981)),
#                    se.fit = TRUE,interval="prediction")


## going forwards don't have another calibration point 
## so we'll calibrate it off the fitted point at 1988
## gradient is the same
## intercept:
1.2167857 - (m1$coefficients[2]/2.368421 * 1988) #-30.25988


m1_output <- m1_pred$fit %>% data.frame() %>% 
  mutate(year = seq(1970,1991,1),
         modelled = ifelse(year %in% c(1982,1983,1984,1985,1986,1987,1988),
                           "observed","modelled"),
         observed = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      0.99,1.02,1.07,1.11,1.15,1.19,1.20,
                      NA,NA,NA),
         fit_gradient = ifelse(year<1982,
                               m1$coefficients[1]/2.412938 + (m1$coefficients[2]/2.368421 * year),
                               ifelse(year>1988,
                                      1.2167857 - (m1$coefficients[2]/2.368421 * 1988) +
                                        (m1$coefficients[2]/2.368421 * year),
                                      fit))) 

m1_output <- m1_output %>% 
  mutate(fit_gradient_multiplier = fit_gradient/fit,
         test = fit * fit_gradient_multiplier,
         lwr_gradient = lwr*fit_gradient_multiplier,
         upr_gradient = upr*fit_gradient_multiplier)


ggplot(m1_output)+
  geom_rect(aes(xmin=1969.5,xmax=1981.5,ymin=-Inf,ymax=Inf),fill="lightgrey")+
  geom_rect(aes(xmin=1988.5,xmax=1991.5,ymin=-Inf,ymax=Inf),fill="lightgrey")+
  geom_line(aes(x=year,y=fit_gradient,linetype="Model fit"))+
  geom_errorbar(data = m1_output %>% filter(modelled!="observed"),
                aes(x=year,ymin=lwr_gradient,ymax=upr_gradient,col="95% prediction intervals"))+
  geom_point(data = m1_output %>% filter(modelled=="modelled"),
             aes(x=year,y=fit_gradient,col="Estimated using our model"))+
  geom_point(aes(x=year,y=observed,col="Raw data"))+
  theme_bw()+
  scale_colour_manual(values=c("blue","blue","black"))+
  scale_fill_manual(values="blue")+
  scale_linetype_manual(values="dashed")+
  labs(x="Year",y="Units used per donation",col="",fill="",linetype="")+
  scale_x_continuous(breaks=seq(min(m1_output$year),max(m1_output$year),1))+
  scale_y_continuous(limits=c(0,1.4),n.breaks=6)+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle=90),
        panel.grid.minor.x = element_blank(),
        legend.box = "vertical")+
  guides(col = guide_legend(order=1,
                            override.aes = list(linetype = c(1, 0, 0),
                                                shape = c(NA,16,16))))
ggsave("Task 2/outputs/SupFigure3.png",width=5,height=4)




#### output for stochastic model


### get the exact binomial CIs for the observed data

observed <- data.frame("year"=c(1982,1983,1984,1985,1986,1987,1988),
                       "n" = c(2059,2136,2159,2119,2128,2094,2141),
                       "x" = c(2047,2183,2308,2347,2448,2490,2579))

observed <- observed %>%
  mutate(pois.exact(x=x,pt=n))


## want to merge these values with the main data frame so we use these lower and upper for the CIs

comb_intervals <- merge(m1_output,
                        observed %>% dplyr::select(year,rate,lower,upper),
                        by="year",all.x = TRUE) %>%
  mutate(lwr_stoc_model = ifelse(is.na(observed),lwr_gradient,
                                 lower),
         upr_stoc_model = ifelse(is.na(observed),upr_gradient,
                                 upper))


write.csv(comb_intervals %>% dplyr::select(year,fit_gradient,lwr_stoc_model,upr_stoc_model),
          "Task 2/outputs/units_per_donation.csv",row.names=FALSE)





