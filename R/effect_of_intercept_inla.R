

inla.int <- inla(N_deaths ~  1 + #intercept
                   f(agey,model = "ar1",  hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))+  #age-specific intercept
                   f(agey2,t,model = "ar1",constr=T, hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) +   #age-varying effect for time
                   f(agey3,pandemic_period,constr=T, model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))+  #age-varying effect for pandemic
                   offset(log.offset), 
                 family = "nbinomial", data = N_deaths_yr,
                 control.fixed = prior_fixed,
                 control.family = list(hyper = prior_hyper),
                 control.compute = list(dic = TRUE, 
                                        waic = TRUE, 
                                        config = TRUE,
                                        return.marginals.predictor = TRUE))

b1$pred1.int <- inla.int$summary.fitted.values[,"mean"]

p0.fitted.int <- ggplot(b1, aes(x=agey*85, y=log(pred1.int/Population*100000), group=t))+
  geom_line( color='red')+
  theme_classic()
p0.fitted.int


inla.no.int <- inla(N_deaths ~  0 + #intercept
                   f(agey,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))+  #age-specific intercept
                   f(agey2,t,model = "ar1", constr=T,hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) +   #age-varying effect for time
                   f(agey3,pandemic_period,constr=T,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))+  #age-varying effect for pandemic
                   offset(log.offset), 
                 family = "nbinomial", data = N_deaths_yr,
                 control.fixed = prior_fixed,
                 control.family = list(hyper = prior_hyper),
                 control.compute = list(dic = TRUE, 
                                        waic = TRUE, 
                                        config = TRUE,
                                        return.marginals.predictor = TRUE))

b1$pred1.no.int <- inla.no.int$summary.fitted.values[,"mean"]

p0.fitted.int +
  geom_line(data=b1,aes(x=agey*85,y=log(pred1.no.int/Population*100000), group=t), color='blue' )


N_deaths_yr_pre <- N_deaths_yr %>%
  filter(year<=2019)

inla.int <- inla(N_deaths ~  0 + #intercept
                   f(agey,model = "ar1",  hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))+  #age-specific intercept
                                      offset(log.offset), 
                 family = "nbinomial", data = N_deaths_yr_pre,
                 control.fixed = prior_fixed,
                 control.family = list(hyper = prior_hyper),
                 control.compute = list(dic = TRUE, 
                                        waic = TRUE, 
                                        config = TRUE,
                                        return.marginals.predictor = TRUE))


re.age <- inla.int$summary.random$agey %>%
  mutate()

p1.age.inla <- ggplot(re.age, aes(x=ID*85, y=exp(mean)))+
  geom_line()+
  theme_classic()+
  geom_ribbon(data=re.age, aes(x=ID*85, ymin=exp(`0.025quant`), ymax=exp(`0.975quant`)), alpha=0.2)
p1.age.inla
