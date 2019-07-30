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

Enrollment_File <- read_csv("../Data/Raw/Closed_Claims/109_Enrollment_File.csv")
Mx_Header <- read.csv("../Data/Raw/Closed_Claims/109_Medical_Claims_Header.csv", stringsAsFactors = F)
Mx_Service_Lines <- read.csv("../Data/Raw/Closed_Claims/109_Medical_Claims_Service_Lines.csv", stringsAsFactors = F)
Rx_Claims <- read.csv("../Data/Raw/Closed_Claims/109_Pharmacy_Claims.csv", stringsAsFactors = F)

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

# reformat Rx claims ... only run these once
# Rx_Claims$patient_dob %<>%  as.Date('%m/%d/%Y')
# Rx_Claims$date_of_service %<>%  as.Date('%m/%d/%Y')
# Rx_Claims$extract_date %<>%  as.Date('%m/%d/%Y')
# Rx_Claims$date_authorized %<>%  as.Date('%m/%d/%Y')
# Rx_Claims$received_date %<>%  as.Date('%m/%d/%Y')






ClosedClaims <- Enrollment_File %>% filter(has_closed_medical_benefit == T & has_closed_pharmacy_benefit == T) #2,516 patients

ClosedClaims <- ClosedClaims %>% left_join(Mx_Header) #166,171 claims

M.ClosedClaims <- ClosedClaims %>% left_join(Mx_Service_Lines) #992,320 claims


# do this for pharmacy claims too at a later date 


save(M.ClosedClaims, file = "../Data/Claims/Medical_Claims.RData")




