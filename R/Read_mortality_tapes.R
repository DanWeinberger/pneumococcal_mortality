#library(feather)
library(data.table)
library(readr)
library(pbapply)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(cdlTools)
library(parquetize)

#the geographic resolution missing from the public data


#https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm
file.names1<- list('MULT2014.USAllCnty.txt','MULT2015.USAllCnty.txt','MULT2016.USAllCnty.txt','MULT2017.USAllCnty.txt','Mort2018US.AllCnty.txt','MULT2019US.AllCnty.txt','MULT2020.USAllCnty.txt','MULT2021US.AllCnty.txt')

all.ds <- lapply(file.names1, function(x){
  d1 <- read_fwf(file=paste0("./CONFIDENTIAL/raw/" ,x),
                 fwf_positions(start=c(20,21,23,28,61,63,64,65,69,102,445,70,71, 79,484,146,167,174,181,188,195,202,209,216,223,230,237,244,251,258,265,272,279,286,293,300, 804,806),
                               end=c(  20,22,25,28,62,63,64,66,69,105,446,  70,73, 80,486,149, 171,178,185,192,199,206,213,220,227,234,241,248,255,262,269,276,283,290,297,304,805, 817),
                               col_names = c('res_status','state','county','county_pop','education1989','education2003','education_flag','month','sex','year','race','age_detail_class','age_detail_number','agec','hispanic', paste0('icd', 1:21 ), 'race_recode2021','occupation' )),
                  guess_max=10000)
  return(d1)
})

all.ds <- lapply(all.ds, function(x){
  x$education1989 = as.character(x$education1989)
  x$education2003 = as.character(x$education2003)
  x$education_flag = as.character(x$education_flag)

  return(x)
  })

 df1 <- bind_rows(all.ds)
saveRDS(df1, './CONFIDENTIAL/compiled_data.rds')

#rds_to_parquet('./CONFIDENTIAL/compiled_data.rds', "R:/NCHS_mortality_GEO/parquet", partition = "no")


df1 <- readRDS('./CONFIDENTIAL/compiled_data.rds')

#take a 1% sample for use in workshop demonstrations
df1_sample <- df1 %>% sample_n( round(0.01*nrow(df1))) %>%
  mutate(age_detail_number = as.numeric(age_detail_number),

         agey= if_else(age_detail_class==1, age_detail_number,
              if_else(    age_detail_class==2,age_detail_number/12,
         if_else(age_detail_class==4,age_detail_number/365,
         if_else(age_detail_class==5,age_detail_number/365/24,
         if_else(age_detail_class==6,age_detail_number/365/24/60,99999))))) , 
         
         hisp_recode = if_else(hispanic<=199 & hispanic>=100,0,
                        if_else(hispanic>=200 & hispanic <=299,1,999)),
         
         race_recode = if_else( race %in% c('01') & hisp_recode!=1 ,1,
                       if_else(race %in% c('02') & hisp_recode!=1 ,2,
                       if_else(race %in% c('03') & hisp_recode!=1 ,4,
                       if_else(race %in% c('04','05','18','28','48','68','78') & hisp_recode!=1 ,5,
                       if_else(race %in% c('06','07','38','58') & hisp_recode!=1 ,5,
                       if_else(hisp_recode==1,3,999))))))
         )%>%
  select(month, sex, year, race_recode, agey,  starts_with('icd')) 
saveRDS(df1_sample,'./Data/mortality_1percent.rds')


#Combine all the ICD codes into a single variable separated by _
df1 <-  df1 %>%
  unite(all_icd, icd1:icd21, na.rm=F)

df1$hisp_recode <- 999
df1$hisp_recode[df1$hispanic<=199 & df1$hispanic>=100] <- 0
df1$hisp_recode[df1$hispanic >=200 & df1$hispanic <= 299] <- 1
#table(df1$hisp_recode)

#RACE: 
#1=Non-Hispanic White
#2=Non-Hispanic- Black
#3= Hispanic
#4-American Indian/Native Alaskan
#5: Asian/Pacific Island

df1$race_recode<- NA
df1$race_recode[df1$hisp_recode == 1] <- 3 #Hispanic

df1$race_recode[df1$race %in% c('01') & df1$hisp_recode != 1] <- 1 #white, non-Hispanic
df1$race_recode[df1$race %in% c('02') & df1$hisp_recode != 1 ]  <- 2 #black, non-Hispanic
df1$race_recode[ df1$race %in% c('03') & df1$hisp_recode != 1 ]  <- 4 #American Indian
df1$race_recode[ df1$race %in% c('04','05','18','28','48' ,'68','78') & df1$hisp_recode != 1]  <- 5 #Asian
df1$race_recode[ df1$race %in% c( '06','07','38','58') & df1$hisp_recode != 1]  <- 5 #Hawaain/Pac Is
df1$race_recode[is.na(df1$race_recode)] <- 999

#RACE RECODE:
#1=Non-Hispanic White
#2=Non-Hispanic- Black
#3= Hispanic
#4-American Indian/Native Alaskan
#5: Asian/Pacific Island

df1$age_group <- df1$agec
df1$agec <- NA
df1$agec[df1$age_group %in% c('01','02')] <- "Under 5 Years"
df1$agec[df1$age_group %in% c('03','04')] <- "5-24 Years"
df1$agec[df1$age_group %in% c('05','06')] <- "25-44 years"
df1$agec[df1$age_group %in% c('07','08')] <- "45-64 years"
df1$agec[df1$age_group %in% c('09')] <- "65-74 years"
df1$agec[df1$age_group %in% c('10')] <- "75-84 years"
df1$agec[df1$age_group %in% c('11')] <- '85 years and older'




## Cause specific deaths

covid.codes <- c('U071','Z28310','Z28311',"Z86.16", "Z28.9","J1282","M3581") #Define codes for COVID-19 https://www.cdc.gov/mmwr/volumes/70/wr/mm7014e2.htm U07.1 probably only relevant one for 2020

pneumococcal.codes <- c('A403','J13','B953','G001')


df1 <- df1 %>%
  mutate(a403 = 1*grepl('A403',all_icd),
         j13 = 1*grepl('J13', all_icd),
         b953 = 1*grepl('B953', all_icd),
         g001 = 1*grepl('G001', all_icd),
         pneumococcal =  1*((a403 + j13 + b953 + g001 )>0)  ,
         covid = grepl('U071', all_icd),
         j154 =1 *grepl('J154', all_icd), #streptococcal pneumonia
         j181 =1 *grepl('J181', all_icd), #unspecified lobar pneumonia
         streptococcal_sepsis = 1* ( grepl('A408',all_icd) + grepl('A409',all_icd) )>0 ) 


df1$agey <- as.numeric(df1$age_detail_number)
df1$agey[df1$age_detail_class==2] <- as.numeric(df1$age_detail_number[df1$age_detail_class==2] )/12
df1$agey[df1$age_detail_class==4] <- as.numeric(df1$age_detail_number[df1$age_detail_class==4] )/365
df1$agey[df1$age_detail_class==5] <- as.numeric(df1$age_detail_number[df1$age_detail_class==5] )/365/24
df1$agey[df1$age_detail_class==6] <- as.numeric(df1$age_detail_number[df1$age_detail_class==6] )/365/24/60



pneumococcal_deaths <- df1 %>%
  filter(pneumococcal==1 | j154==1 |j181==1  )

#hist(df1$agey[df1$rsv==1 & df1$agey<1])


df1 %>%
  group_by(year) %>%
  summarize(pneumococcal=sum(pneumococcal) , streptococcal_sepsis=sum(streptococcal_sepsis))


saveRDS(pneumococcal_deaths,'./CONFIDENTIAL/pneumococcal_deaths_line_list_geo.rds')

pneumococcal_deaths_nogeo <- pneumococcal_deaths %>%
  select(-state, -county, -county_pop)

saveRDS(pneumococcal_deaths_nogeo,'./data/pneumococcal_deaths_line_list_no_geo.rds')

