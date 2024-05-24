
death_plot_fun <- function(disease='ipd',reg.form='N_deaths_pre ~ index+ sin12+cos12 +sin6 +cos6', ...){
  
  grouping_vars <- quos(...)
  disease_sym <- ensym(disease)
  
  plot.ds1 <- pneumo.deaths.adult %>% 
    filter(!!disease_sym == 1) %>%
    group_by( !!!grouping_vars ) %>%
    summarize(N_deaths=n()) %>%
    ungroup() %>%
    mutate(year=as.factor(year),month=as.numeric(as.character(month))) %>%
    tidyr::complete( !!!grouping_vars, fill=list(N_deaths=0) ) 
    
  
  plot.cols <- c(rep('gray85',6),'#66c2a5', '#fc8d62','#8da0cb')
  
  p1 <- ggplot(plot.ds1, aes(x=month, y=N_deaths, group=year, col=year)) +
    geom_line() +
    theme_classic() +
    scale_color_manual(values=plot.cols)+
    ylim(0, NA)
  
  
  ds1 <- plot.ds1 %>%
    mutate(date=as.Date(paste(year, month, '01', sep='-')),
           qtr=as.factor(lubridate::quarter(date)),
           index= interval('2014-01-01', date) %/% months(1) ,
           sin12 = sin(2*pi*index/12),
           cos12 = cos(2*pi*index/12),
           sin6 = sin(2*pi*index*2/12),
           cos6 = cos(2*pi*index*2/12),
           year=as.numeric(year),
           N_deaths_pre= if_else(date >='2020-03-01', NA_real_,N_deaths),
           log_offset=log(1))
  
  #Poisson
  form1 <- as.formula(reg.form)
  mod1 <- glm(reg.form,
              data=ds1, family='poisson')
  
  preds <- pred_interval(mod=mod1,ds.glm=ds1)
  
  ds1a <- cbind.data.frame(ds1,preds)%>%
    mutate(rr_median=N_deaths/pred_median, rr_lcl=N_deaths/pred_lcl, rr_ucl=N_deaths/pred_ucl)
  
  p1_a <- ggplot(data=ds1a, aes(x=date, y=N_deaths)) +
    geom_line() +
    theme_classic()+
    geom_ribbon(aes(ymin=pred_lcl, ymax=pred_ucl), alpha=0.2)+
    geom_line(aes(x=date, y=pred_median), col='red', lty=2, alpha=0.2)+
    geom_vline(xintercept= as.Date(c('2020-03-01','2021-08-01')), lty=3, col='gray') +
    annotate("text", x=as.Date(c('2020-04-01','2021-09-01')), y=175, label=c('March 2020', 'August 2021'), angle=90, col='gray')+
    ylab('N deaths')+
    ggtitle(disease)
  
  rr.plot <- 
    ggplot(data=ds1a, aes(x=date, y=rr_median)) +
    geom_line() +
    theme_classic()+
    geom_ribbon(aes(ymin=rr_lcl, ymax=rr_ucl), alpha=0.2)+
    # ylim(0.5,2.2) +
    coord_cartesian(ylim = c(0.5, 2.2))+
    geom_hline(yintercept=1, lty=2, col='red')+    
    geom_vline(xintercept= as.Date(c('2020-03-01','2021-08-01')), lty=3, col='gray') +
    annotate("text", x=as.Date(c('2020-04-01','2021-09-01')), y=1.5, label=c('March 2020', 'August 2021'), angle=90, col='gray')
  out.list = list('plot1'=p1, 'ts'=plot.ds1,'ts.plot'=p1_a,'rr.plot'=rr.plot)
  
  return(out.list)
}

