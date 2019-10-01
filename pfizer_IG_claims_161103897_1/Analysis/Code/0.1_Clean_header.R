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

library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)
library(multidplyr)
library(doParallel)
`%notin%` = function(x,y) !(x %in% y)
# The Claims Header file has the admin and discharge date in it.  That makes it a whole lot easier.  You only need to include for the first round of analysis, for each IG treatment claim:
#   •	The IG treatment claim
# •	The header file for that IG treatment claim
# •	The header file and service line file for any claim that falls on the same date as the IG treatment 
# •	The header file only for any claim where discharge date is after admin date AND admin date <  IG treatment date < discharge date

rx_sample <- read_csv("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/rx_sample.csv")

rx_sample <- rx_sample %>% filter(transaction_type == "PAID")
rx_sample$reject_code_1 %>% table() # E9 = missing provider ID, all other reject codes all missing
rx_sample <- rx_sample %>% filter(is.na(reject_code_1))
rx_sample$ndc11 <- rx_sample$ndc11 %>% str_remove_all("XX")

# find IG treatments 
IG_codes<- read_excel("~/OneDrive - Kantar/Projects/Pfizer IG/Pfizer_IG_Open_Claims_Inclusion_Codes.xlsx", 
                                                    col_types = c("text", "text", "skip"))

IG_jcodes <- IG_codes$`J Codes`[-1] %>% na.omit()


IG_codes$NDC <- IG_codes$NDC %>% str_remove_all("-")
IG_codes$NDC <- IG_codes$NDC %>% str_remove_all("XX")
IG_NDC <- IG_codes$NDC

OHDSI <- read_delim("~/Kantar/Arunajadai, Srikesh (KH) - KHDICT/OHDSI/CONCEPT.csv", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)
OHDSI_NDC <- OHDSI %>% filter(vocabulary_id == "NDC")
OHDSI_HCPCS <- OHDSI %>% filter(vocabulary_id == "HCPCS")


##  get codes for Jcodes
jcodes <- OHDSI_HCPCS %>% filter(concept_code %in% IG_jcodes) 
NDCs <- OHDSI_NDC %>% filter(concept_code %in% IG_NDC)


## get codes for NDC codes
NDCs <- lapply(IG_codes$NDC, function(x){grep(x, OHDSI_NDC$concept_code, value = T, ignore.case = T)})
NDCs <- unlist(NDCs)
    
save(IG_jcodes, NDCs, file = "~/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/IG_codes.RData")

   # 00944270001 does not exist
rx_sample2  <- rx_sample %>% group_by(patient_id) %>% 
  arrange(patient_id, date_of_service) %>% 
  mutate(NewPt_id = group_indices()) %>% ungroup() %>% 
  group_by(NewPt_id, as.character(date_of_service))%>% 
  mutate(Visit_id = group_indices()) %>% ungroup() %>% 
  group_by(NewPt_id, Visit_id) %>% 
  mutate(IG_NDC_visit = case_when(NDC %in% NDCs ~ 1,
                                  TRUE ~ 0)) %>% ungroup()


# all have IG claims 



#save(rx_sample2, file = "~/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Pharmacy_claims.RData")
          
load("C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med_merged.RData")
header2[header2 == ""]<- NA
service2[service2 == ""]<- NA



# service2$rendering_prov_npi <- service2$rendering_prov_npi %>% as.character()
# service2$service_facility_npi <- service2$service_facility_npi %>% as.character()
# service2$ordering_pr_npi <- service2$ordering_pr_npi %>% as.character()
# service2$referring_pr_npi <- service2$referring_pr_npi %>% as.character()


#service <- fread('C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/service_sample.csv', header = T, sep = ',')

header3 <- header2 %>% filter(claim_id %in% service2$claim_id) %>% select(claim_id, patient_id)

service2 <- service %>% left_join() %>%  select(patientid, received_date, claim_id, service_to, service_from, 
                                ndc_code, service_line_number, procedure, units, 
                                date_of_service,place_of_service, rendering_prov_npi, 
                                purchased_service_npi, service_facility_npi, 
                                supervising_prov_npi, ordering_pr_npi)

med <- header2 %>% left_join(service2)
med$claim_date <- med$claim_date %>% as.Date("%m/%d/%Y")
med$admission_date <- med$admission_date %>% as.Date("%m/%d/%Y")
med$claim_date <- med$claim_date %>% as.Date("%m/%d/%Y")
med$service_from %<>% as.Date("%Y-%m-%d")
med$service_to %<>% as.Date("%Y-%m-%d")

save(med, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Med_merged.RData")

med_group <- 


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

save(med, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Visits.RData")


med3 <- med2 %>% filter(!is.na(patient_id))
 
med4 <- med3 %>% select(patient_id, NewPt_id, Visit_id, Visit_type) %>% distinct() 


try2 <- try %>% mutate(attending_npi = as.character(attending_npi),
                        Kantar_npi = case_when(is.na(attending_npi) ~ billing_pr_npi,
                                       TRUE ~ attending_npi))



try3 <- try2 %>% rename(NPI = Kantar_npi) %>% left_join(NPIregistry) %>% arrange(patient_id, claim_id, claim_date)

try4 <- try3 %>% group_by(patient_id) %>% 
  arrange(patient_id, claim_date) %>% 
  mutate(NewPt_id = group_indices()) 

try5 <- try4 %>% group_by(NewPt_id, as.character(claim_date)) %>% mutate(Visit_id = group_indices())

save(try5, file = "C:/Users/balkaranb/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Visits.RData")
               
#save(service2, file = "~/OneDrive - Kantar/Projects/Pfizer IG/Data/Clean/Service.RData")


service <- fread('read_csv("D:/Kantar_Health_Division/Pfizer_IG_Claims_161103897_1/_sample.csv")', header = T, sep = ',')

service2 <- service %>%  select(patientid, received_date, claim_id, service_to, service_from, 
                                ndc_code, service_line_number, procedure, units, 
                                date_of_service,place_of_service, rendering_prov_npi, 
                                purchased_service_npi, service_facility_npi, 
                                supervising_prov_npi, ordering_pr_npi)
service2[service2 == ""]<- NA
service2 <- service2 %>% group_by(patientid) %>% 
  arrange(patientid, date_of_service) %>% 
  mutate(NewPt_id = group_indices()) %>% ungroup() %>% 
  group_by(NewPt_id, as.character(date_of_service))%>% 
  mutate(Visit_id = group_indices()) %>% ungroup() %>% 
  group_by(NewPt_id, Visit_id) %>%  
  mutate(Visit_type = case_when(ndc_code %in% NDCs & is.na(procedure) ~ "IG_NDC_Visit",
                                procedure %in% IG_jcodes & ndc_code %in% NDCs ~ "IG_J_NDC_Visit",
                                procedure %in% IG_jcodes & is.na(ndc_codes) ~"Unspedified Jcode visit",
                                procedure %notin% IG_jcodes & ndc_code %notin% NDCs ~ "non_IG_visit")) %>% 
  ungroup()




# Merge Header and service

