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
library(readxl)
library(lubridate)
library(data.table)
library(magrittr)
library(multidplyr)
library(doParallel)
`%notin%` = function(x,y) !(x %in% y)
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Visits.RData")
med$discharge_date %<>% as.Date()
med$date_of_service %<>% as.Date()

hosp <- med %>% filter(claim_type_code == "I" | discharge_date > admission_date)

med2 <- med %>% select(patient_id, claim_id, date_of_service, NewPt_id, Visit_id)
med3 <- med2 %>% group_by(patient_id, claim_id, date_of_service)
med4 <- med3 %>% distinct()
med4 %>% filter(!is.na(patient_id))
PIDs <- med4$patient_id %>% unique()
NPIDs <- med4$NewPt_id %>% unique()
VIDs <- med4$Visit_id %>% unique()
med5 <- med4 %>% left_join(select(med, patient_id, claim_id, 
                                  date_of_service, NewPt_id, Visit_id, Visit_type))
 
J_NDC_IG <- med5 %>% filter(Visit_type == "IG_J_NDC_Visit") # 103,216
IG_NDC <- med5 %>% filter(Visit_type == "IG_NDC_Visit")  
IG_NDC2 <- IG_NDC %>% group_by(patient_id) %>% filter(claim_id %notin% J_NDC_IG$claim_id)
non_IG <- med5 %>% filter(Visit_type == "non_IG_visit")
Unspecified_jcode <- med5 %>% filter(Visit_type == "Unspedified Jcode visit")

save(IG_NDC, J_NDC_IG, non_IG, Unspecified_jcode, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Visit_datasets.RData")


save(med5, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Visit_types.RData")
#med4 %>% filter(is.na(patient_id) & is.na(date_of_service)) 27 claims with no patient ID and no date of service




Med_grouped <- Med %>% group_by(NewPt_id, Visit_id, Visit_type) %>% select(-patient_id, -claim_id) 

Med_grouped2 <- Med_grouped %>% unique()

IG_NDC_per_pt <- Med_grouped2 %>% ungroup() %>% select(NewPt_id, IG_NDC_Visit)%>% 
  group_by(NewPt_id) %>% tally()
Types <- Types %>% left_join(med)

med <- med %>% filter(claim_id %in% Types$claim_id)


J_NDC_per_pt <- Med_grouped2 %>% ungroup() %>% select(NewPt_id, J_NDC_Visit)%>% 
  group_by(NewPt_id) %>% tally()

Jcode_per_pt <- Med_grouped2 %>% ungroup() %>% select(NewPt_id, Unspecified_J_Visit)%>% 
  group_by(NewPt_id) %>% tally()


load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med_merged.RData")
med$date_of_service %<>% as.Date()
med <- select(med, patient_id, claim_id, claim_type_code, date_of_service, place_of_service,
              billing_pr_npi, attending_npi, referring_pr_npi, facility_npi)
gc()
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Visit_datasets.RData")
rm(non_IG)
gc()
rm(IG_NDC)
gc()

IG_NDC$date_of_service %<>% as.Date()

gc()
J_NDC_IG$date_of_service %<>% as.Date()

gc()
Unspecified_jcode$date_of_service %<>% as.Date()
gc()


IG_NDC2 <- IG_NDC %>% ungroup() %>%
  left_join(med) %>% unique()

save(IG_NDC2, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/IG_NDC_NPI.RData")

J_NDC_IG <- J_NDC_IG %>% ungroup() %>% unique()
save(med, J_NDC_IG, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/J_NDC_IG_med_NOT MERGED.RData")
#Restart everything to clear RAM
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/J_NDC_IG_med_NOT MERGED.RData")
gc()
  J_NDC_IG <- J_NDC_IG %>% left_join(med) 
  gc()
dat1 <- J_NDC_IG[1:1000000, ] 
dat2 <- J_NDC_IG[1000001:2000000,]
dat3 <- J_NDC_IG[2000001:3000000,]
dat4 <- J_NDC_IG[3000001:4000000,]
dat5 <- J_NDC_IG[4000001:5000000,]
dat6 <- J_NDC_IG[5000001:6000000,]
dat7 <- J_NDC_IG[6000001:7000000,]
dat8 <- J_NDC_IG[7000001:8000000,]
dat9 <- J_NDC_IG[8000001:9000000,]
dat10 <- J_NDC_IG[9000001:10000000,]
dat11 <- J_NDC_IG[10000001:11000000,]
dat12 <- J_NDC_IG[11000001:12000000,]
dat13 <- J_NDC_IG[12000001:13000000,]
dat14 <- J_NDC_IG[13000001:14000000,]
dat15 <- J_NDC_IG[14000001:15000000,]
dat16 <- J_NDC_IG[15000001:16000000,]
dat17 <- J_NDC_IG[16000001:17000000,]
dat18 <- J_NDC_IG[17000001:18000000,]
dat19 <- J_NDC_IG[18000001:18068658,]


datlist <- list(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, 
             dat11, dat12, dat13, dat14, dat15, dat16, dat17, dat18,dat19)
dat_list <- map(datlist, unique)
rm(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, 
   dat11, dat12, dat13, dat14, dat15, dat16, dat17, dat18,dat19)
J_NDC <-  plyr::ldply(dat_list, data.table)
  rm(med)
  J_NDC <- J_NDC %>% unique()
  gc()
save(J_NDC, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/J_NDC_IG_NPI.RData")

  