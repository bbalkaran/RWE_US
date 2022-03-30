#########################################################
#  Script: 0.2_Xtabs_for_NPI.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 9/12/19
#  Date Edited: 9/12/19
########################################################

library(tidyverse)
library(data.table)
library(magrittr)

`%notin%` = function(x,y) !(x %in% y)
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Visits.RData")
med$discharge_date %<>% as.Date()
med$date_of_service %<>% as.Date()

med <- med %>% select(patient_id, claim_id, claim_type_code, date_of_service, service_from,
                      service_to, attending_npi, billing_pr_npi, facility_npi,
                      place_of_service, 
                      procedure, ndc_code, Visit_type)


med <- med %>% filter(!is.na(patient_id)) # remove records with no patient id
med <- med %>% filter(!is.na(date_of_service)) 
# remove recorxs with no date of servive, # = 35469022 service lines



# look at range of dates: remove outliers
    # date of service
med$date_of_service %>% summary()
med %>% filter(date_of_service < "2000-01-01") %>% select(date_of_service) %>% View()
med <- med %>% filter(date_of_service >= "2000-01-01")
med <- med %>% filter(!date_of_service == "9999-12-31")
# dates of service from 1914, 1915, 1920, 1970, 1990, 1992
# remove these --> likely DOB 

med$patient_id %>% n_distinct()
  med$claim_id %>% n_distinct()
# claims = 7,439,607
# patinets = 221,108



# count those with P and  I claims 
df2 <- med %>% select(patient_id, claim_type_code) %>%
  group_by(patient_id) %>%
  count(claim_type_code)

claims <- df2 %>% spread( claim_type_code, n)

claims2 <- claims %>% 
  mutate(all_I = case_when(is.na(P) & is.na(`<NA>`) & I >0  ~ 1,
                           TRUE ~ 0))

Inst_claims_only <- claims2 %>% filter(all_I == 1) %>% 
  select(patient_id) %>% distinct() # patients with inst claims only
Inst_claims_only$patient_id %>% n_distinct()
#save for later when comparing hosp vs non hosp
# 70,656 with inst claims only

save(Inst_claims_only, claims2, 
     file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Institutional_Claims_1.RData")
med <- med %>% filter(patient_id %notin% Inst_claims_only$patient_id)
med$patient_id %>% n_distinct()
# 145447 patients
gc()
rm(claims, claims2, Inst_claims_only, df2)
gc()


hosp2<- data.table(select(med, patient_id, claim_id, date_of_service, Visit_type))



df <- hosp2 %>% arrange(patient_id, date_of_service) %>%
  group_by(factor(patient_id)) %>%
  mutate(previous = lag(date_of_service, 1)) %>%
  mutate(tae.1 = date_of_service - lag(date_of_service, 1))%>% 
  ungroup() %>%
  mutate(consecutive_flag = case_when(df$tae.1 == 1 ~ 1)) %>%
  mutate(consecutive_flag = case_when(tae.1 == 1 ~ 1,
                                      lead(tae.1 == 1)~1))
df2 <- df %>% group_by(patient_id, date_of_service) %>% 
  filter(consecutive_flag == 1)

df3 <- df %>% filter(patient_id %in% df2$patient_id)

df4 <- df3 %>% group_by(patient_id, date_of_service) %>% 
  fill(consecutive_flag) %>% 
  fill(consecutive_flag, .direction = "up") # RAN THIS OVERNIGHT TAKES A LONG TIME!!

save(df4, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Consectutive visits identifier.RData")
 
gc()
rm(hosp2)
gc()
rm(df)

# select out those with continuous dates and with an IG visit --> counting these as hospitalizations
df5 <- df4 %>% group_by(patient_id, date_of_service) %>% 
  mutate(consecutive_dates_w_IG = case_when((Visit_type == "IG_J_NDC_Visit" | 
                                              Visit_type == "IG_NDC_Visit") & 
                                              consecutive_flag == 1 ~ "IG hospitalization",
                                            (Visit_type == "Unspedified Jcode visit" &
                                               consecutive_flag == 1) ~ "IG hospitalization",
                                            consecutive_flag == 1 ~ "Hospitalization",
                                            TRUE ~ "Other Visit"))

gc()
rm(df4)
gc()

df5 %>% group_by(consecutive_dates_w_IG) %>% summarise(n_patients= n_distinct(patient_id))

ID_IG_hospitalizatons <- df5 %>% select(patient_id, consecutive_dates_w_IG) %>%
  group_by(patient_id) %>%
  count(consecutive_dates_w_IG) %>%
  spread( consecutive_dates_w_IG, n) 


Patients_hosp <- ID_IG_hospitalizatons %>% filter(!is.na(Hospitalization)) %>%
  filter(!is.na(`IG hospitalization`) & is.na(`Other Visit`)) 
#7,612 patients with hospitalizations based on consecutive dates and IG claims

save(Patients_hosp, ID_IG_hospitalizatons, df5, 
     file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Institutional_claims_2.RData")

# remove these patients with hospitalizations
med <- med %>% filter(patient_id %notin% Patients_hosp$patient_id)
med$patient_id %>% n_distinct()
med$claim_id %>% n_distinct()
# 137113 patients


# remove non-IG claims
# claims = 4,729,036
#patients  = 192,040

med <- med %>% filter(!Visit_type == "non_IG_visit")
med$patient_id %>% n_distinct()
#137,130
med$claim_id %>% n_distinct()

  

save(med, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med Claims for Site Identification.RData")
# go to 0.3_Xtabs_for_med_NPI.R




  