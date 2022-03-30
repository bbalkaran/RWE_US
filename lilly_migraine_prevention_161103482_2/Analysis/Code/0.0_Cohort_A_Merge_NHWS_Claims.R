#########################################################
#  Script: Clean_Claims.R
#  Purpose: Clean claims files for analysis 
#  Data file: 
#  Author: Bridget Balkaran
#  Project:161103482-2
#  Project Manager: Janelle Cambron-Mellet
#  Date Created: 7/26/19
#  Date Edited: 8/19/19
########################################################

# 1. workspace -----

library(tidyverse)
library(magrittr)
library(openxlsx)
library(lubridate)
library(odbc)
library(dbplyr)
library(DBI)
library(kollekt)
options(scipen = 999)

# 2. medical claims -----
Enrollment_File <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Enrollment_File.csv")
Mx_Header <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Medical_Claims_Header.csv")
Mx_Service_Lines <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Medical_Claims_Service_Lines.csv")
Rx_Claims <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Pharmacy_Claims.csv")

# reformat header... only run these once: using magrittr here
Mx_Header$admission_date %<>% as.Date('%Y-%m-%d')
Mx_Header$discharge_date %<>% as.Date('%Y-%m-%d')
Mx_Header$claim_date %<>% as.Date('%Y-%m-%d')
Mx_Header$file_date %<>% as.Date('%Y-%m-%d')
Mx_Header$patient_dob %<>% as.Date('%Y-%m-%d')
Mx_Header$received_date %<>% as.Date('%Y-%m-%d')

#reformat service line .... only run these once
Mx_Service_Lines$service_to %<>% as.Date('%m/%d/%Y')
Mx_Service_Lines$service_from %<>%  as.Date('%m/%d/%Y')
Mx_Service_Lines$file_date %<>%  as.Date('%m/%d/%Y')
Mx_Service_Lines$date_of_service %<>%  as.Date('%m/%d/%Y')
Mx_Service_Lines$received_date %<>%  as.Date('%m/%d/%Y')


# 2A. Clean enrollment file -----
Enrollment_File <- Enrollment_File %>% filter(has_closed_medical_benefit== T & has_closed_pharmacy_benefit == T) # all closed 2,516



# 2B. Clean header file -----

# select out migraine diagnoses

ICD_fields <- list("d1","da", "d2", "d3", "d4","d5", "d6", "d7",
                   "d8", "d9", "d10", "d11", "d12", "d13", "d14",
                   "d15", "d16", "d17", "d18","d19",
                   "d20", "d21", "d22", "d23", "d24", "d25", "d26")
get_migraine_ICDs <- lapply(ICD_fields, function(x) {pull(Mx_Header, x)}) %>% unlist()

migraine_ICD10 <- grep("G43", get_migraine_ICDs, value = T, ignore.case = T)
migraine_ICD9 <- grep("^[3][4][6]", get_migraine_ICDs, value = T)
migraine_ICDs <- c(migraine_ICD10, migraine_ICD9)

Mx_Header <- Mx_Header %>% filter(client_patient_id %in% Enrollment_File$client_patient_id)

# Migraine dx 
Mx_Header <- Mx_Header %>% filter(d1 %in% migraine_ICDs |
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
                       d26 %in% migraine_ICDs) # 1,263 claims

Mx_H_2017 <- Mx_Header %>% filter(claim_date >= "2017-01-01" & claim_date <= "2017-12-31" )
Mx_H_2017$client_patient_id %>% n_distinct()
  # 186 claims,  68 patients


Mx_Service_Lines <- Mx_Service_Lines %>% filter(claim_id %in% Mx_H_2017$claim_id)
Mx_Service_Lines$client_patient_id %>% n_distinct() # 68
Mx_Service_Lines$service_to %>% range(na.rm = T)
Mx_Service_Lines <- Mx_Service_Lines %>% filter(service_from >= "2017-01-01" & service_from <= "2017-12-31" )
Mx_Service_Lines$client_patient_id %>% n_distinct() # 68
# # check with Jack's file 
# JR_Med <- read_csv("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Migraine_Prev_cohort_Medical_JR.csv", 
#                    col_types = cols(X1 = col_skip(), 
#                                     claim_date = col_date(format = "%Y-%m-%d"),
#                                     file_date = col_date(format = "%Y-%m-%d")))
# JR_Med$client_patient_id %>% n_distinct() # 68 patients
# # all same patients?
# JR_Med %>% filter(!client_patient_id  %in% Migraine_Header_2017$client_patient_id)
# Migraine_Header_2017 %>% filter(!client_patient_id %in% JR_Med$client_patient_id) # yep, all same
# 
# # check different claim
# JR_Med %>% filter(!claim_id %in% Migraine_Header_2017$claim_id) %>% View() # only one different
# Migraine_Header_2017 %>% filter(!claim_id %in% JR_Med$claim_id) # all same


# 3. Pharmacy claims -----
Rx_Claims <- read_csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Raw/Closed_Claims/109_Pharmacy_Claims.csv")
Triptan_and_Ergot_Meds <- read.csv("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Medications_Lists/Triptan_and_Ergot_Meds.csv", stringsAsFactors = F)
triptans <- grep("triptan", Triptan_and_Ergot_Meds$Kantar.Grouping, value = T)
ergots <- grep('ergot', Triptan_and_Ergot_Meds$Kantar.Grouping, value = T, ignore.case = T)
inclusion_meds <- Triptan_and_Ergot_Meds %>% filter(Kantar.Grouping %in% triptans | Kantar.Grouping %in% ergots) # not all NDCs 11 digits, need to cross-ref w/ OHDSI

project_path <- file.path(getPath('KHDICT'),'OHDSI')
file_name <- file.path(project_path,'CONCEPT.csv')
scan(file_name, what = character())
OHDSI <- read_delim("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - KHDICT/OHDSI/CONCEPT.csv",
                    "\t", escape_double = FALSE, trim_ws = TRUE)

OHDSI <- OHDSI %>% filter(vocabulary_id == "NDC")

ndcs <- c(grep("dihydroergot", OHDSI$concept_name, value = T, ignore.case = T),
             grep("almotriptan", OHDSI$concept_name, value = T, ignore.case = T),
             grep("Eletriptan", OHDSI$concept_name, value = T, ignore.case = T),
             grep("Ergotamine", OHDSI$concept_name, value = T, ignore.case = T),
             grep("Frovatriptan", OHDSI$concept_name, value = T, ignore.case = T),
             grep("Sumatriptan", OHDSI$concept_name, value = T, ignore.case = T),
             grep("Naratriptan", OHDSI$concept_name, value = T, ignore.case = T),
             grep("Rizatriptan", OHDSI$concept_name, value = T, ignore.case = T),
             grep("Zolmitriptan", OHDSI$concept_name, value = T, ignore.case = T))

migraine_NDCs <- OHDSI %>% filter(concept_name %in% ndcs)
dropthese <- grep("belladonna", migraine_NDCs$concept_name, value = T, ignore.case = T)
migraine_NDCs <- migraine_NDCs %>% filter(!concept_name %in% dropthese )

# reformat  claims ... only run these once
Rx_Claims$patient_dob %<>%  as.Date('%m/%d/%Y')
Rx_Claims$date_prescription_written %<>% as.Date('%m/%d/%Y')
Rx_Claims$date_of_service %<>%  as.Date('%m/%d/%Y')
Rx_Claims$extract_date %<>%  as.Date('%m/%d/%Y')
Rx_Claims$date_authorized %<>%  as.Date('%m/%d/%Y')
Rx_Claims$received_date %<>%  as.Date('%m/%d/%Y')

Rx_Claims <- Rx_Claims %>% filter(is.na(reject_code_1) & is.na(reject_code_2) & 
                                    is.na(reject_code_3) & is.na(reject_code_4) & is.na(reject_code_5))# 129,752 claims
Rx_Claims <- Rx_Claims %>% filter(transaction_type == "PAID") # 108,367 claims
Rx_Claims$client_patient_id %>% n_distinct() # 680


         
# Inclusion_Rx_Claims <- Rx_Claims%>% filter(client_patient_id %in% Enrollment_File$client_patient_id) %>%
#   filter(ndc11 %in% inclusion_meds$code | ndc11 %in% migraine_NDCs$concept_code) 
Rx_Claims <- Rx_Claims %>% filter(date_of_service >= "2017-01-01" & date_of_service <= "2017-12-31") # 427 claims, 73 patients; without OHDSI NCDs
Rx_Claims$client_patient_id %>% n_distinct()    # 461

# # check with Jack's file
# 
# 
# JR_Pharm <- read_csv("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Migraine_Prev_cohort_Pharma_JR.csv", 
#                      col_types = cols(X1 = col_skip(), date_of_service = col_date(format = "%m/%d/%Y"), 
#                                       date_prescription_written = col_date(format = "%m/%d/%Y"), 
#                                       patient_dob = col_date(format = "%m/%d/%Y"), 
#                                       timestamp_authorized = col_datetime(format = "%m/%d/%Y %H:%M")))   
# 
# Check_pharm <- JR_Pharm %>% filter(!client_patient_id %in% Enrollment_File$client_patient_id)
# 
# Check_NDCs <- JR_Pharm %>% filter(! ndc11 %in% inclusion_meds$code | !ndc11 %in% migraine_NDCs$concept_code)
# 
# Check_NDCs2 <- OHDSI %>% filter(concept_code %in% Check_NDCs$ndc11)# puling in 114 meds not triptan or ergot



# 4. add in bridging file to claims-----

Bridge <- read_delim("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - KHDICT/LINKER/komodo/komodo_bridge_06202019.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

Bridge <- Bridge %>% mutate(client_patient_id = `Komodo ID`)


Mx_H_2017<- Mx_H_2017 %>% left_join(Bridge) 
Med_2017 <- Mx_H_2017 %>% full_join(Mx_Service_Lines)
Med_2017$client_patient_id %>% n_distinct() # 68

Rx_Claims <- Rx_Claims %>% left_join(Bridge) 
Rx_Claims$client_patient_id %>% n_distinct() # 461


rm(Enrollment_File, ICD_fields, Mx_Header, Mx_Service_Lines,OHDSI)



# 5 determine linked patients -----
z <- dbConnect(odbc::odbc(), driver = "MySQL ODBC 5.3 ANSI Driver", 
               database = "NHWS", uid = "bridgetb", pwd = "bridgetb", server = "10.176.50.61", 
               pORt = 3306)
us_date <- dbGetQuery(z, "select * from char_all
                                 where variable like 'start_time'
                                 or variable like 'date'")
arrange_and_transpose_str <- function(dat){
  dat %>% arrange(zKey) %>% spread(variable, strvalue)
}

us_date <- arrange_and_transpose_str(us_date)
us_date <- us_date %>% mutate(START_TIME  = case_when(is.na(START_TIME) ~ DATE,
                                                      TRUE ~ START_TIME))
us_date <- us_date %>% filter(source == "us_2017")
us_date$START_TIME <- date(as.Date(us_date$START_TIME))
us_date <- us_date %>% select(-DATE, -uniqueid)
us_date  <- us_date %>% rename(zkey = zKey)


load("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseI_clean.RData")

dat_clean <- dat_clean %>% filter(zkey %in% Bridge$zkey)
dat_clean$source <- dat_clean$source %>% as.character() %>% str_trim("both")
dat_clean <- dat_clean %>% left_join(us_date, by = c("zkey", "source"))

b <- dat_clean %>% left_join(Med_2017) %>% full_join(Rx_Claims)   475

b <- b %>% mutate(NHWSindex_date = START_TIME) 
b$client_patient_id %>% is.na() %>% table() 
b <- b %>% filter(!is.na(client_patient_id))
b$client_patient_id %>% n_distinct() #474
b$zkey %>% n_distinct() #474
NHWS_Med_Pharm <- b %>% group_by(client_patient_id, source) %>% filter(row_number(NHWSindex_date)==1) %>% 
  arrange(client_patient_id, source) 
# 64 patients  if we use claims bound within jan 1st 2017 - dec 31st 2017

# go to 0.1 for COhort B (1 year window around NHWS date), ignore code below


# df <- df %>%
#   group_by(card_id) %>%
#   arrange(purchase_date) %>%
#   mutate(diff = purchase_date - lag(purchase_date, default = first(purchase_date))) %>%
#   mutate(diff = round(diff/86400, digits = 2))
# 
# 
# Linked_Migraine_Header_2017 <- Linked_Migraine_Header_2017 %>% group_by(client_patient_id) %>% arrange(claim_date) %>% 
#   mutate(date_diff = claim_date - lag(claim_date)) %>% 
#   arrange(client_patient_id, claim_date, date_diff) 
# 
# Linked_Migraine_Header_2017$claim <- "m"
# Linked_Migraine_Header_2017 %>% select(client_patient_id, claim_id, claim_date, date_diff) %>% View()
# 
# 
# # linked Rx patients
# Linked_Inclusion_Rx_Claims <- Linked_Inclusion_Rx_Claims %>% group_by(client_patient_id) %>% arrange(date_of_service) %>% 
#   mutate(p.date.diff = date_of_service - lag(date_of_service)) %>% 
#   arrange(client_patient_id, date_of_service, p.date.diff)
# 
# Linked_Inclusion_Rx_Claims$claim <- "p"
# 
# 
# # merge header and Rx
# 
# Linked_Claims <- Linked_Migraine_Header_2017 %>% 
#   select(client_patient_id, zkey,  source, claim_id, claim_date) %>% #date_diff, claim) %>% 
#   full_join(select(Linked_Inclusion_Rx_Claims, client_patient_id, zkey, source, claim_id, 
#                   date_of_service)) %>% #p.date.diff, claim)) %>% 
#   arrange(client_patient_id, zkey, source, claim_date, date_of_service)
# write.xlsx(Linked_Claims, file= "~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Linked_Claims.xlsx")
# 
# 
# Linked_Claims_Inclusion_Identifier <- read_excel("C:/Users/balkaranb/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Linked_Claims_Inclusion_Identifier.xlsx", 
#                                                  col_types = c("numeric", "numeric", "text", 
#                                                                "text", "text", "date", "text", "text", 
#                                                                "date", "text", "numeric", "text"))
# 
# MG <- Linked_Claims_Inclusion_Identifier %>% filter(Keep == 1) # 541 claims
# MG$client_patient_id %>% n_distinct() # 73 patients
# MG$zkey %>% n_distinct() # 73 patients
# 
# 
# 
# #save(MG, file = "~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseII.RData")
# 
# load("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseII.RData")
# load("~/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseII.RData")

