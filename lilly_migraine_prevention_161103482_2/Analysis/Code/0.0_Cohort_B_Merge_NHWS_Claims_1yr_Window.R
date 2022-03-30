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


Mx_H_2017 <- Mx_Header #%>% filter(claim_date >= "2017-01-01" & claim_date <= "2017-12-31" )
Mx_H_2017$client_patient_id %>% n_distinct()
  # 204 patients


Mx_Service_Lines <- Mx_Service_Lines %>% filter(claim_id %in% Mx_H_2017$claim_id)
Mx_Service_Lines$client_patient_id %>% n_distinct() # 204
Mx_Service_Lines$service_to %>% range(na.rm = T)
#Mx_Service_Lines <- Mx_Service_Lines %>% filter(service_from >= "2017-01-01" & service_from <= "2017-12-31" )
Mx_Service_Lines$client_patient_id %>% n_distinct() # 204
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


rm(ergots, inclusion_meds, Triptan_and_Ergot_Meds, triptans)

# 4. add in bridging file to claims-----

Bridge <- read_delim("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - KHDICT/LINKER/komodo/komodo_bridge_06202019.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

Bridge <- Bridge %>% mutate(client_patient_id = `Komodo ID`)


Mx_H_2017<- Mx_H_2017 %>% left_join(Bridge) 
Med_2017 <- Mx_H_2017 %>% full_join(Mx_Service_Lines)
Med_2017$client_patient_id %>% n_distinct() # 204

Rx_Claims <- Rx_Claims %>% left_join(Bridge) 
colnames(Rx_Claims) <- paste("p", colnames(Rx_Claims), sep = ".")
Rx_Claims <- Rx_Claims %>% rename(client_patient_id = p.client_patient_id,
                                  zkey = p.zkey)
Rx_Claims$client_patient_id %>% n_distinct() # 680
Rx_Claims$zkey %>% n_distinct()




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
dbDisconnect(z)

load("C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/PhaseI_clean.RData")

dat_clean <- dat_clean %>% filter(zkey %in% Bridge$zkey)
dat_clean$source <- dat_clean$source %>% as.character() %>% str_trim("both")
dat_clean <- dat_clean %>% left_join(us_date, by = c("zkey", "source"))
rm(us_date)

rm(Bridge)


drop2 <- c("p.timestamp_authorized","p.group_id", 
           "p.diagnosis_code", "p.diagnosis_code_type",          
           "p.ndc11_original", "p.quantity_prescribed_original",
           "p.is_service", "p.prior_authorization_type_code",
           "p.is_compound_drug", "p.level_of_service",             
           "p.coupon_number", "p.coupon_type",                 
           "p.coupon_value_amount", "p.pharmacy_submitted_cost",       
           "p.patient_pay","p.copay_coinsurance",            
           "p.reject_code_1", "p.reject_code_2" ,"p.reject_code_3", 
           "p.reject_code_4","p.reject_code_5", "p.extract_date" )

Rx_Claims <- Rx_Claims %>% select(-drop2)

drop3 <- c("orig_claim_id", "assignment_benefits_indicator",   
           "release_of_information_indicator",   "diagnosis_e_code_1", 
           "diagnosis_e_code_2", "diagnosis_group", "p1", "p2",                              
           "p3", "p4",  "p5", "p6", "p7", "p8", "p9", "p10",                             
           "p11", "p12", "p13", "p14", "p15", "p16",                             
           "p17", "p18", "p19", "p20", "p21", "p22",                             
           "p23", "p24", "p25", "payment_type", "patient_amount_paid",
           "revenue_center_code", "revenue_code")

Med_2017 <- Med_2017 %>% select(-drop3)

b <- dat_clean %>% left_join(Med_2017)
c <- dat_clean %>% full_join(Rx_Claims)


b <- b %>% mutate(NHWSindex_date = START_TIME) 
b$client_patient_id %>% is.na() %>% table() 
b <- b %>% filter(!is.na(client_patient_id))
b$client_patient_id %>% n_distinct() #193
b <- b %>% mutate(preNHWS = (NHWSindex_date - months(6)),
                  postNHWS = (NHWSindex_date + months(6)))
b <- b %>% filter((claim_date >= preNHWS & claim_date <= postNHWS) | 
                    (date_of_service >= preNHWS & date_of_service <= postNHWS))
b$zkey %>% n_distinct() #65

# allow 1 year windowaround NHWS date 
c <- c %>% mutate(NHWSindex_date = START_TIME) 
c$client_patient_id %>% is.na() %>% table() 
c <- c %>% filter(!is.na(client_patient_id))
c$client_patient_id %>% n_distinct() #680
c$zkey %>% n_distinct() #193
c <- c %>% mutate(preNHWS = (NHWSindex_date - months(6)),
             postNHWS = (NHWSindex_date + months(6)))
c <- c %>% filter(p.date_of_service >= preNHWS & p.date_of_service <= postNHWS)
c$client_patient_id %>% n_distinct() #438


save(b, c,file = "C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/Data to merge.RData")


NHWS_Med_Pharm <- b %>% full_join(c)
NHWS_Med_Pharm$client_patient_id %>% n_distinct() #450
NHWS_Med_Pharm$zkey %>% n_distinct() # 450


NHWS_Med_Pharm <- NHWS_Med_Pharm %>% group_by(client_patient_id, source) %>% filter(row_number(NHWSindex_date)==1) %>% 
  arrange(client_patient_id, source) 

CohortB1 <- NHWS_Med_Pharm
CohortB1 <- CohortB1 %>% ungroup()
CohortB1$DEPEMP_R <- CohortB1$DEPEMP_R %>% 
  fct_collapse(`Employed full time` = c("Employed full time "),
               `Employed part time` = "Employed part time ",
               `Self-employed` = "Self-employed ",
               `Not employed` = c("Homemaker ", "Student ",
                                  "Disability (Long-Term/Short-Term) ",
                                  "Not employed, but looking for work ",
                                  "Not employed and not looking for work "),
               `Retired` = "Retired ")

save(CohortB1,file = "C:/Users/BalkaranB/Kantar/Arunajadai, Srikesh (KH) - RWE_US/Lilly_migraine_prevention_161103482_2/CohortB1.RData")

# 450 patientsif we use claims with a migraine DX bound within a 1 year window around NHWS date

# create claims derived variables


