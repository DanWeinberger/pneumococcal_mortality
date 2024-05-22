
death_plot_fun <- function(disease='pneumococcal',...){
  
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
  
  out.list = list('plot1'=p1, 'ts'=plot.ds1)
  
  return(out.list)
}

