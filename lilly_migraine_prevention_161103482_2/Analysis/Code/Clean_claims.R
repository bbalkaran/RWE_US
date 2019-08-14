#########################################################
#  Script: Clean_Claims.R
#  Purpose: Clean claims files for analysis 
#  Data file: 
#  Author: Bridget Balkaran
#  Project:161103482-2
#  Project Manager: Janelle Cambron-Mellet
#  Date Created: 7/26/19
#  Date Edited: 7/29/19
########################################################

# 1. workspace -----

library(tidyverse)
library(magrittr)
options(scipen = 999)

# 2. medical claims -----
Enrollment_File <- read_csv("./Analysis/Data/Raw/Closed_Claims/109_Enrollment_File.csv")
Mx_Header <- read.csv("./Analysis/Data/Raw/Closed_Claims/109_Medical_Claims_Header.csv", stringsAsFactors = F)
Mx_Service_Lines <- read.csv("./Analysis/Data/Raw/Closed_Claims/109_Medical_Claims_Service_Lines.csv", stringsAsFactors = F)
Rx_Claims <- read.csv("./Analysis/Data/Raw/Closed_Claims/109_Pharmacy_Claims.csv", stringsAsFactors = F)

# reformat header... only run these once: using magrittr here
# Mx_Header$admission_date %<>% as.Date('%Y-%m-%d')
# Mx_Header$discharge_date %<>% as.Date('%Y-%m-%d')
# Mx_Header$claim_date %<>% as.Date('%Y-%m-%d')
# Mx_Header$file_date %<>% as.Date('%Y-%m-%d')
# Mx_Header$patient_dob %<>% as.Date('%Y-%m-%d')
# Mx_Header$received_date %<>% as.Date('%Y-%m-%d')

#reformat service line .... only run these once
# Mx_Service_Lines$service_to %<>% as.Date('%m/%d/%Y')
# Mx_Service_Lines$service_from %<>%  as.Date('%m/%d/%Y')
# Mx_Service_Lines$file_date %<>%  as.Date('%m/%d/%Y')
# Mx_Service_Lines$date_of_service %<>%  as.Date('%m/%d/%Y')
# Mx_Service_Lines$received_date %<>%  as.Date('%m/%d/%Y')


# 2A. Clean enrollment file -----
Enrollment_File <- Enrollment_File %>% filter(has_closed_medical_benefit== T & has_closed_pharmacy_benefit == T) # all closed



# 2B. Clean header file -----

# select out migraine diagnoses
Mx_Header$d1 %>% grep("G43")

migraine_ICD10 <- grep("G43", Mx_Header$d1, value = T, ignore.case = T)
migraine_ICD9 <- grep("346", Mx_Header$d1, value = T)

migraine_ICDs <- c(migraine_ICD10, migraine_ICD9)

Migraine_Header <- Mx_Header %>% filter(d1 %in% migraine_ICDs |
                       da %in% migraine_ICDs |
                       d2 %in% migraine_ICDs | 
                       d3 %in% migraine_ICDs | 
                       d4 %in% migraine_ICDs | 
                       d5 %in% migraine_ICDs | 
                       d6 %in% migraine_ICDs | 
                       d7 %in% migraine_ICDs | 
                       d8 %in% migraine_ICDs | 
                       d9 %in% migraine_ICDs | 
                       d10 %in% migraine_ICDs | 
                       d11 %in% migraine_ICDs | 
                       d12 %in% migraine_ICDs | 
                       d13 %in% migraine_ICDs | 
                       d14 %in% migraine_ICDs | 
                       d15 %in% migraine_ICDs | 
                       d16 %in% migraine_ICDs | 
                       d17 %in% migraine_ICDs | 
                       d18 %in% migraine_ICDs | 
                       d19 %in% migraine_ICDs | 
                       d20 %in% migraine_ICDs |
                       d21 %in% migraine_ICDs | 
                       d22 %in% migraine_ICDs | 
                       d23 %in% migraine_ICDs | 
                       d24 %in% migraine_ICDs | 
                       d25 %in% migraine_ICDs | 
                       d26 %in% migraine_ICDs) %>% as_tibble()

Migraine_Header_2017 <- Migraine_Header %>% filter(claim_date >= "2017-01-01" & claim_date <= "2017-12-31" )
  # 215 claims, 84 patients





# do this for pharmacy claims too at a later date 


#save(M.ClosedClaims, file = "../Data/Claims/Medical_Claims.RData")







# 3. Pharmacy claims -----
Rx_Claims <- read_csv("./Analysis/Data/Raw/Closed_Claims/109_Pharmacy_Claims.csv")
Triptan_and_Ergot_Meds <- read_csv("Analysis/Data/Medications_Lists/Triptan_and_Ergot_Meds.csv")
triptans <- grep("triptan", Triptan_and_Ergot_Meds$`Kantar Grouping`, value = T)
ergots <- grep('ergot', Triptan_and_Ergot_Meds$`Kantar Grouping`, value = T, ignore.case = T)
inclusion_meds <- Triptan_and_Ergot_Meds %>% filter(`Kantar Grouping` %in% triptans | `Kantar Grouping` %in% ergots)



# reformat  claims ... only run these once
Rx_Claims$patient_dob %<>%  as.Date('%m/%d/%Y')
Rx_Claims$date_prescription_written %<>% as.Date('%m/%d/%Y')
Rx_Claims$date_of_service %<>%  as.Date('%m/%d/%Y')
Rx_Claims$extract_date %<>%  as.Date('%m/%d/%Y')
Rx_Claims$date_authorized %<>%  as.Date('%m/%d/%Y')
Rx_Claims$received_date %<>%  as.Date('%m/%d/%Y')

Rx_Claims <- Rx_Claims %>% filter(is.na(reject_code_1) & is.na(reject_code_2) & 
                                    is.na(reject_code_3) & is.na(reject_code_4) & is.na(reject_code_5))# 129,752 claims
Rx_Claims <- Rx_Claims %>% filter(transaction_type == "PAID")
         
Inclusion_Rx_Claims <- Rx_Claims%>% filter(ndc11 %in% inclusion_meds$code)
Inclusion_Rx_Claims_2017 <- Inclusion_Rx_Claims %>% filter(date_of_service >= "2017-01-01" & date_of_service <= "2017-12-31") # 427 claims, 73 patients









