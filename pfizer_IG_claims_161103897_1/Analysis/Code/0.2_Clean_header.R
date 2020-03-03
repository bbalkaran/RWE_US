#########################################################
#  Script: 0.1_Clean_header.R
#  Purpose: NPI data from CMS
#  Data file: 
#  Author: Bridget Balkaran
#  Project: 161103897-1
#  Project Manager: Martine Maculaitis
#  Date Created: 9/12/19
#  Date Edited: 9/12/19
########################################################

# 1 workspace -----
library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)

`%notin%` = function(x,y) !(x %in% y)
load("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer Home Infusion/Data/Header.RData")
header2[header2 ==""] <- NA
header2 <- header2 %>% distinct()
load("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer Home Infusion/Data/Service.RData")
service2[service2 == ""]<- NA
service2 <- service2 %>% distinct()
load("C:/Users/BalkaranB/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/IG_codes.RData")
# The Claims Header file has the admin and discharge date in it.  That makes it a whole lot easier.  You only need to include for the first round of analysis, for each IG treatment claim:
#   •	The IG treatment claim
# •	The header file for that IG treatment claim
# •	The header file and service line file for any claim that falls on the same date as the IG treatment 
# •	The header file only for any claim where discharge date is after admin date AND admin date <  IG treatment date < discharge date


# all have IG claims 


header <- header %>% filter(claim_id %in% service2$claim_id)


med <- header %>% left_join(service2)
med$claim_date <- med$claim_date %>% as.Date("%m/%d/%Y")
med$admission_date <- med$admission_date %>% as.Date("%m/%d/%Y")
med$claim_date <- med$claim_date %>% as.Date("%m/%d/%Y")
med$service_from %<>% as.Date("%Y-%m-%d")
med$service_to %<>% as.Date("%Y-%m-%d")


med <- med %>% group_by(patient_id) %>% 
  arrange(patient_id, date_of_service) %>% 
  mutate(NewPt_id = group_indices()) %>% ungroup() 



med <- med %>% 
  group_by(NewPt_id, as.character(date_of_service))%>% 
  mutate(Visit_id = group_indices()) %>% ungroup() 

med <- med %>% 
  group_by(NewPt_id, Visit_id) %>%  
  mutate(Visit_type = case_when(ndc_code %in% NDCs & is.na(procedure) ~ "IG_NDC_Visit",
                                procedure %in% IG_jcodes & ndc_code %in% NDCs ~ "IG_J_NDC_Visit",
                                procedure %in% IG_jcodes & is.na(ndc_code) ~"Unspedified Jcode visit",
                                (procedure %notin% IG_jcodes) & (ndc_code %notin% NDCs) ~ "non_IG_visit")) %>% 
  ungroup() 


save(med, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer Home Infusion/Data/Med_merged.RData")


# Merge Header and service

