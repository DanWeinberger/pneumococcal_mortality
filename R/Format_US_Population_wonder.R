
#library(feather)
library(data.table)
library(readr)
library(pbapply)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)

Format_US_Population_wonder <- function(){
  #these are all July 1 estimates of popsize for the year
 d1a <-  read.csv('./Data/wonder_pop/Single-Race Population Estimates 2010-2019 by State and Single-Year Age.txt', header=T, sep="\t")
 d1b <-  read.csv('./Data/wonder_pop/Single-Race Population Estimates 2020-2021 by State and Single-Year Age.txt', header=T, sep="\t")
 
 d1 <- bind_rows(d1a,d1b) %>%
   filter(Notes == '') %>%
   mutate( agec= if_else(Five.Year.Age.Groups.Code %in% c('1','1-4'), "Under 5 Years",
                  if_else(Five.Year.Age.Groups.Code %in% c('5-9','10-14','15-19','20-24'), "5-24 Years",
                  if_else(Five.Year.Age.Groups.Code %in% c('25-29','30-34','35-39','40-44'),"25-44 years",
                  if_else(Five.Year.Age.Groups.Code %in% c('45-49','50-54','55-59','60-64'),"45-64 years",
                  if_else(Five.Year.Age.Groups.Code %in% c('65-69','70-74'),"65-74 years",
                  if_else(Five.Year.Age.Groups.Code %in% c('75-79','80-84'),"75-84 years",
                  if_else(Five.Year.Age.Groups.Code %in% c('85+'),'85 years and older', NA_character_
                                      )        )))))),
          race_recode = if_else(Ethnicity=='Hispanic or Latino', 3,
                          if_else(Race=='White',1,
                          if_else(Race=="Black or African American" ,2,
                          if_else( Race=="American Indian or Alaska Native",4,
                          if_else(Race %in% c('Asian',"Native Hawaiian or Other Pacific Islander"),5,999)))))
   )
           
 
  #RACE: 
  #1=Non-Hispanic White
  #2=Non-Hispanic- Black
  #3= Hispanic
  #4-American Indian/Native Alaskan
  #5: Asian/Pacific Island
  
  # RACE_RECODE: 
  #   1=Non-Hispanic White
  # 2=Non-Hispanic- Black
  # 3= Hispanic
  # 4=American Indian/Native Alaskan
  # 5= Asian/Pacific Island
  # 999=Missing
  

  d2 <- d1 %>%
    rename(region=Region, year=Yearly.July.1st.Estimates) %>%
    group_by(agec, region, race_recode, year) %>%
    summarize(pop=sum(Population)) %>%
    ungroup()
  
  d2$month <- 7
  
  months <- cbind.data.frame('month'=1:12)
  
  pop2a <- bind_rows(d2, months)
  
  pop3 <- pop2a %>%
    tidyr::complete( month, nesting(year,agec,region, race_recode), fill=list(pop=NA) ) %>%
    mutate(date= as.Date(paste(year, month, '01', sep='-'))) %>%
    filter(date>='2013-07-01')
  
  
  filled_pop2 <- pop3 %>%
    group_by(agec , race_recode, region) %>%
    arrange(agec , race_recode,region,year, month) %>%
    #mutate(time=seq(1,n())) %>%  #FIX THIS--wont be correct for 2013
    mutate(time= year + month/12 - 1/12 ) %>%
    mutate(pop.interpol=approx(time,pop,time)$y) %>%
    ungroup()
  
  filled_pop2 <- filled_pop2[,c('agec','race_recode','region','date','pop.interpol')]
  
  saveRDS(filled_pop2,'./Data/pop_interpol.rds')
  
  
}
