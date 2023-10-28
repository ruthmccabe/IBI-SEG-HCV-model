### uncertainty in the infectious prevalence in 1991

prevs <- rbeta(n = 10000, shape1 = 533, shape2 =  808938)*100
clearances <- 1 - rnorm(n = 10000, mean = 0.26, sd = 0.018)

comb <- prevs*clearances

quantile(comb,c(0.025,0.5,0.975))
