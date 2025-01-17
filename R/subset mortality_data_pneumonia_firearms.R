library(arrow)
library(dplyr)
pneumonia_deaths <-  open_dataset("R/compiled_data.parquet", format = "parquet")  %>%
  filter(grepl("J09|J10|J11|J12|J13|J14|J15|J16|J17|J18|J19|J20|J21|J22", all_icd)) %>%
 collect()

names(pneumonia_deaths)

pneumonia_deaths %>%
  mutate(date=as.Date(year, month, '01'),
         agey=round(agey)) %>%
  group_by(date, race_recode_alt)%>%
  collect()

write_parquet(pneumonia_deaths,'./Data/pneumonia_deaths_pq')

# W34: Accidental discharge of other and unspecified firearm.
# X72: Intentional self-harm by handgun discharge.
# X73: Intentional self-harm by rifle, shotgun, and larger firearm discharge.
# X74: Intentional self-harm by other and unspecified firearm discharge.
# X93: Assault by handgun discharge.
# X94: Assault by rifle, shotgun, and larger firearm discharge.
# X95: Assault by other and unspecified firearm discharge.
# Y22: Firearm discharge, undetermined intent.
# Y23: Other firearm discharge, undetermined intent.
# Y24: Unspecified firearm discharge, undetermined intent.
# Y35.0: Legal intervention involving firearm discharge.
firearm_deaths <-  open_dataset("R/compiled_data.parquet", format = "parquet")  %>%
  filter(grepl("W34|X72|X73|X74|X93|X94|X95|Y22|Y23|Y24|Y35", all_icd)) %>%
  collect()

write_parquet(firearm_deaths,'./Data/firearm_deaths_pq')



open_dataset('./Data/pneumonia_deaths_pq')%>%
  mutate(date=as.Date(year, month, '01'),
         agey=round(agey)) %>%
  group_by(date, race_recode_alt)%>%
  summarize(N=n()) %>%
  collect()
