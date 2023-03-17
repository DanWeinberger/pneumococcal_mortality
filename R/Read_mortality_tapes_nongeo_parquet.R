#library(feather)
library(data.table)
library(readr)
library(pbapply)
library(dplyr)
library(parquetize)
library(reshape2)
library(tidyr)
library(pbapply)
library(arrow)
library(duckdb)

library(ggplot2)
library(cdlTools)

#the geographic resolution missing from the public data


##https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm

# file.names1<- list('VS14MORT.DUSMCPUB','VS15MORT.DUSMCPUB','VS16MORT.DUSMCPUB','VS17MORT.DUSMCPUB','Mort2018US.PubUse.txt','VS19MORT.DUSMCPUB_r20210304','VS20MORT.DUSMCPUB_r20220105','mort2021us.txt' )
# 
# all.ds <- pblapply(file.names1, function(x){
#   d1 <- read_fwf(file=paste0("R:/NCHS_mortality_public/" ,x),
#                  fwf_positions(start=c(20,61,63,64,65,69,102,445,70,71, 79,484,146,167,174,181,188,195,202,209,216,223,230,237,244,251,258,265,272,279,286,293,300, 806),
#                                end=c(  20,62,63,64,66,69,105,446,  70,73, 80,486,149, 171,178,185,192,199,206,213,220,227,234,241,248,255,262,269,276,283,290,297,304, 817),
#                                col_names = c('res_status','education1989','education2003','education_flag','month','sex','year','race','age_detail_class','age_detail_number','agec','hispanic', paste0('icd', 1:21 ), 'occupation' )),
#                   guess_max=10000)
#   return(d1)
# })
# 
# all.ds <- pblapply(all.ds, function(x){
#   x$education1989 = as.character(x$education1989)
#   x$education2003 = as.character(x$education2003)
#   x$education_flag = as.character(x$education_flag)
# 
#   return(x)
#   })
# 
#  df1 <- bind_rows(all.ds) %>%
#          mutate( all_icd = paste(icd1,icd2,
#                                  icd3,icd4,
#                                  icd5,icd6,
#                                  icd7,icd8,
#                                  icd9,icd10,
#                                  icd11,icd12,
#                                  icd13,icd14,
#                                  icd15,icd16,
#                                  icd17,icd18,
#                                  icd19,icd20,
#                                  icd21, sep='_'),
#                  hisp_recode = if_else(hispanic<=199 & hispanic>=100,0,
#                                        if_else( hispanic >=200 & hispanic <= 299,2,999
#                                        )),
#                  race_recode= if_else(hisp_recode == 1,3,
#                                       if_else(race %in% c('01') & hisp_recode != 1,1,
#                                               if_else(race %in% c('02') & hisp_recode != 1 ,2,
#                                                       if_else(race %in% c('03') & hisp_recode != 1,4,
#                                                               if_else(race %in% c('04','05','18','28','48' ,'68','78') & hisp_recode != 1,5,
#                                                                       if_else(race %in% c( '06','07','38','58') & hisp_recode != 1,5,999))
#                                                       )))),
#              
#                  a403 = 1*grepl('A403',all_icd),
#                  j13 = 1*grepl('J13', all_icd),
#                  b953 = 1*grepl('B953', all_icd),
#                  g001 = 1*grepl('G001', all_icd),
#                  pneumococcal =  1*((a403 + j13 + b953 + g001 )>0)  ,
#                  covid = grepl('U071', all_icd),
#                  j154 =1 *grepl('J154', all_icd), #streptococcal pneumonia
#                  j181 =1 *grepl('J181', all_icd), #unspecified lobar pneumonia
#                  streptococcal_sepsis = 1* ( grepl('A408',all_icd) + grepl('A409',all_icd) )>0,
# 
#                  age_detail_number = as.numeric(age_detail_number),
# 
#                  agey = if_else(age_detail_class==2, age_detail_number/12,
#                                 if_else(age_detail_class==4, age_detail_number/365,
#                                         if_else(age_detail_class==5, age_detail_number/365/24,
#                                                 if_else(age_detail_class==6, age_detail_number/365/24/60, age_detail_number
#                                                 ))))
#                            )
# saveRDS(df1, 'R:/NCHS_mortality_public/compiled_data.rds')
 
#Write as a parquet dataset for more efficient access and data parsing
#rds_to_parquet('R:/NCHS_mortality_public/compiled_data.rds', "R:/NCHS_mortality_public/parquet", partition = "no")


## Cause specific deaths

covid.codes <- c('U071','Z28310','Z28311',"Z86.16", "Z28.9","J1282","M3581") #Define codes for COVID-19 https://www.cdc.gov/mmwr/volumes/70/wr/mm7014e2.htm U07.1 probably only relevant one for 2020

pneumococcal.codes <- c('A403','J13','B953','G001')

#RACE: 
#1=Non-Hispanic White
#2=Non-Hispanic- Black
#3= Hispanic
#4-American Indian/Native Alaskan
#5: Asian/Pacific Island

pneumococcal_deaths <-  open_dataset("R:/NCHS_mortality_public/parquet", format = "parquet") %>%
   filter(pneumococcal==1 | j154==1 |j181==1  ) %>%
  collect() 

#hist(df1$agey[df1$rsv==1 & df1$agey<1])




