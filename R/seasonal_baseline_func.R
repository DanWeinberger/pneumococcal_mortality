
seasonal_baseline_func <- function(ds0){
  ds1 <- ds0 %>%
    mutate(date=as.Date(paste(year, month, '01', sep='-')),
           qtr=as.factor(lubridate::quarter(date)),
           index= interval('2014-01-01', date) %/% months(1) ,
           sin12 = sin(2*pi*index/12),
           cos12 = cos(2*pi*index/12),
           sin6 = sin(2*pi*index*2/12),
           cos6 = cos(2*pi*index*2/12),
           N_deaths_pre= if_else(year %in% '2020', NA_real_,N_deaths),
           log_offset=log(1))
  
  #Poisson
  # mod1 <- glm(N_deaths_pre ~ index+ sin12+cos12 +sin6 +cos6,
  #             data=ds1, family='poisson')
  
  #Negative binomial
  
  
  mod1 <- glm.nb(N_deaths_pre ~ index+ sin12+cos12 + sin6 +cos6,
                 data=ds1)
  
  ds1a <- cbind.data.frame(ds1,pred_interval(mod=mod1,model.type='negbin'))
  
  dates_vline <- as.Date(c("2020-03-01", "2021-08-01"))                 # Define positions of vline
  dates_vline <- which(ds1a$date %in% dates_vline)
  
  
  p1 <- ggplot(data=ds1a, aes(x=date, y=N_deaths)) +
    geom_line() +
    theme_classic()+
    geom_line(aes(x=date, y=pred_median), lty=3, col='red')+
    geom_ribbon(aes(ymin=pred_lcl, ymax=pred_ucl), alpha=0.1) +
    geom_vline(xintercept=as.numeric(ds1a$date[dates_vline]), lty=2, col='gray')+
    annotate("text", x=as.Date('2020-03-22'), y=max(ds1a$pred_median)*1.5, label="March 2020", angle=90, size=4, color="darkgray") +
    annotate("text", x=as.Date('2021-08-22'), y=max(ds1a$pred_median)*1.5, label="August 2021", angle=90, size=4, color="darkgray") +
    ylab('Number of deaths') +
    xlab('Date')
  
  ds1a <- ds1a %>%
    mutate(rr_median=N_deaths/pred_median, rr_lcl=N_deaths/pred_ucl, rr_ucl=N_deaths/pred_lcl,
           
           rr_ucl=if_else(rr_ucl>2.2,2.199, rr_ucl),
           rr_lcl=if_else(rr_lcl<0.5,0.5001,rr_lcl))
  
  
  p2 <- ggplot(data=ds1a, aes(x=date, y=rr_median)) +
    geom_line() +
    theme_classic()+
    geom_ribbon(aes(ymin=rr_lcl, ymax=rr_ucl), alpha=0.2)+
    ylim(0.5,2.2) +
    geom_hline(yintercept=1, lty=2, col='red')
  
  out.list=list('ts.plot'=p1, 'rr.plot'=p2)
  return(out.list)
}